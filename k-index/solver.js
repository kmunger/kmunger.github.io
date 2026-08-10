var solver = (function () {
    'use strict';

    /**
     * Represents a solution to a linear programming problem.
     */
    class Solution {
        constructor(tableau, evaluation, feasible, bounded) {
            this.feasible = feasible;
            this.evaluation = evaluation;
            this.bounded = bounded;
            this._tableau = tableau;
            this.solutionSet = {};
        }
        /**
         * Generate the solution set mapping variable IDs to their values.
         */
        generateSolutionSet() {
            const solutionSet = {};
            const tableau = this._tableau;
            const varIndexByRow = tableau.varIndexByRow;
            const variablesPerIndex = tableau.variablesPerIndex;
            const matrix = tableau.matrix;
            const width = tableau.width;
            const rhsColumn = tableau.rhsColumn;
            const lastRow = tableau.height - 1;
            const roundingCoeff = Math.round(1 / tableau.precision);
            for (let r = 1; r <= lastRow; r += 1) {
                const varIndex = varIndexByRow[r];
                const variable = variablesPerIndex[varIndex];
                if (variable === undefined || variable.isSlack === true) {
                    continue;
                }
                const varValue = matrix[r * width + rhsColumn];
                solutionSet[variable.id] =
                    Math.round((Number.EPSILON + varValue) * roundingCoeff) / roundingCoeff;
            }
            return solutionSet;
        }
    }
    /**
     * Represents a solution to a mixed-integer programming problem.
     * Extends Solution with branch-and-cut iteration tracking.
     */
    class MilpSolution extends Solution {
        constructor(tableau, evaluation, feasible, bounded, branchAndCutIterations) {
            super(tableau, evaluation, feasible, bounded);
            this.iter = branchAndCutIterations;
        }
    }

    class BranchMinHeap {
        constructor(initialCapacity = 64) {
            this.heap = new Array(initialCapacity);
            this.size = 0;
            this.seqCounter = 0;
            this.pool = new Array(64);
            this.poolSize = 0;
        }
        allocEntry(branch, seq) {
            if (this.poolSize > 0) {
                const entry = this.pool[--this.poolSize];
                entry.branch = branch;
                entry.seq = seq;
                return entry;
            }
            return { branch, seq };
        }
        freeEntry(entry) {
            if (this.poolSize < 256) {
                this.pool[this.poolSize++] = entry;
            }
        }
        get length() {
            return this.size;
        }
        isEmpty() {
            return this.size === 0;
        }
        clear() {
            this.size = 0;
            this.seqCounter = 0;
        }
        // Compare: returns true if a should be before b (a has higher priority)
        isBefore(a, b) {
            if (a.branch.relaxedEvaluation !== b.branch.relaxedEvaluation) {
                return a.branch.relaxedEvaluation < b.branch.relaxedEvaluation;
            }
            // LIFO tie-breaking: higher seq (more recent) comes first
            return a.seq > b.seq;
        }
        push(branch) {
            const heap = this.heap;
            let idx = this.size;
            this.size++;
            // Grow if needed
            if (idx >= heap.length) {
                heap.length = heap.length * 2;
            }
            const entry = this.allocEntry(branch, this.seqCounter++);
            // Bubble up
            while (idx > 0) {
                const parentIdx = (idx - 1) >> 1;
                const parent = heap[parentIdx];
                if (!this.isBefore(entry, parent)) {
                    break;
                }
                heap[idx] = parent;
                idx = parentIdx;
            }
            heap[idx] = entry;
        }
        pop() {
            if (this.size === 0) {
                return undefined;
            }
            const heap = this.heap;
            const poppedEntry = heap[0];
            const result = poppedEntry.branch;
            this.size--;
            // Return entry to pool
            this.freeEntry(poppedEntry);
            if (this.size === 0) {
                return result;
            }
            // Move last element to root and bubble down
            const last = heap[this.size];
            let idx = 0;
            const halfSize = this.size >> 1;
            while (idx < halfSize) {
                let childIdx = (idx << 1) + 1;
                let child = heap[childIdx];
                const rightIdx = childIdx + 1;
                if (rightIdx < this.size && this.isBefore(heap[rightIdx], child)) {
                    childIdx = rightIdx;
                    child = heap[rightIdx];
                }
                if (!this.isBefore(child, last)) {
                    break;
                }
                heap[idx] = child;
                idx = childIdx;
            }
            heap[idx] = last;
            return result;
        }
        peek() {
            return this.size > 0 ? this.heap[0].branch : undefined;
        }
    }

    function createCut$2(type, varIndex, value) {
        return { type, varIndex, value };
    }
    function createBranch$2(relaxedEvaluation, cuts) {
        return { relaxedEvaluation, cuts };
    }
    function createBranchAndCutService() {
        const applyCuts = (tableau, branchingCuts) => {
            var _a;
            tableau.restore();
            tableau.addCutConstraints(branchingCuts);
            tableau.simplex();
            if ((_a = tableau.model) === null || _a === void 0 ? void 0 : _a.useMIRCuts) {
                // Optimization: reuse previous "after" as next "before" to avoid redundant computation
                let fractionalVolume = tableau.computeFractionalVolume(true);
                while (fractionalVolume > 0) {
                    tableau.applyMIRCuts();
                    tableau.simplex();
                    const fractionalVolumeAfter = tableau.computeFractionalVolume(true);
                    if (fractionalVolumeAfter >= 0.9 * fractionalVolume) {
                        break;
                    }
                    fractionalVolume = fractionalVolumeAfter;
                }
            }
        };
        const branchAndCut = (tableau) => {
            var _a, _b, _c, _d, _e;
            const branches = new BranchMinHeap();
            let iterations = 0;
            const tolerance = (_b = (_a = tableau.model) === null || _a === void 0 ? void 0 : _a.tolerance) !== null && _b !== void 0 ? _b : 0;
            let toleranceFlag = true;
            let terminalTime = 1e99;
            if ((_c = tableau.model) === null || _c === void 0 ? void 0 : _c.timeout) {
                terminalTime = Date.now() + tableau.model.timeout;
            }
            let bestEvaluation = Infinity;
            let bestBranch = null;
            const bestOptionalObjectivesEvaluations = [];
            // Cache optionalObjectives reference to avoid repeated property lookups in hot loop
            const optionalObjectives = tableau.optionalObjectives;
            const nOptionalObjectives = optionalObjectives.length;
            for (let oInit = 0; oInit < nOptionalObjectives; oInit += 1) {
                bestOptionalObjectivesEvaluations.push(Infinity);
            }
            const branch = createBranch$2(-Infinity, []);
            let acceptableThreshold;
            branches.push(branch);
            while (!branches.isEmpty() && toleranceFlag === true && Date.now() < terminalTime) {
                if ((_d = tableau.model) === null || _d === void 0 ? void 0 : _d.isMinimization) {
                    acceptableThreshold = tableau.bestPossibleEval * (1 + tolerance);
                }
                else {
                    acceptableThreshold = tableau.bestPossibleEval * (1 - tolerance);
                }
                if (tolerance > 0) {
                    if (bestEvaluation < acceptableThreshold) {
                        toleranceFlag = false;
                    }
                }
                const activeBranch = branches.pop();
                if (activeBranch.relaxedEvaluation >= bestEvaluation) {
                    continue;
                }
                const cuts = activeBranch.cuts;
                applyCuts(tableau, cuts);
                iterations++;
                if (tableau.feasible === false) {
                    continue;
                }
                const evaluation = tableau.evaluation;
                if (evaluation > bestEvaluation) {
                    continue;
                }
                if (evaluation === bestEvaluation) {
                    let isCurrentEvaluationWorse = true;
                    for (let o = 0; o < nOptionalObjectives; o += 1) {
                        const currentCost = optionalObjectives[o].reducedCosts[0];
                        const bestCost = bestOptionalObjectivesEvaluations[o];
                        if (currentCost > bestCost) {
                            break;
                        }
                        else if (currentCost < bestCost) {
                            isCurrentEvaluationWorse = false;
                            break;
                        }
                    }
                    if (isCurrentEvaluationWorse) {
                        continue;
                    }
                }
                if (tableau.isIntegral() === true) {
                    tableau.__isIntegral = true;
                    if (iterations === 1) {
                        tableau.branchAndCutIterations = iterations;
                        return;
                    }
                    bestBranch = activeBranch;
                    bestEvaluation = evaluation;
                    for (let oCopy = 0; oCopy < nOptionalObjectives; oCopy += 1) {
                        bestOptionalObjectivesEvaluations[oCopy] =
                            optionalObjectives[oCopy].reducedCosts[0];
                    }
                    if ((_e = tableau.model) === null || _e === void 0 ? void 0 : _e.keep_solutions) {
                        const nowSolution = tableau.model.tableau.getSolution();
                        const store = nowSolution.generateSolutionSet();
                        store.result = nowSolution.evaluation;
                        if (!tableau.model.solutions) {
                            tableau.model.solutions = [];
                        }
                        tableau.model.solutions.push(store);
                    }
                }
                else {
                    if (iterations === 1) {
                        tableau.save();
                    }
                    const variable = tableau.getMostFractionalVar();
                    const varIndex = variable.index;
                    const varValue = variable.value;
                    const cutsHigh = [];
                    const cutsLow = [];
                    const nCuts = cuts.length;
                    for (let c = 0; c < nCuts; c += 1) {
                        const cut = cuts[c];
                        if (cut.varIndex === varIndex) {
                            if (cut.type === "min") {
                                cutsLow.push(cut);
                            }
                            else {
                                cutsHigh.push(cut);
                            }
                        }
                        else {
                            cutsHigh.push(cut);
                            cutsLow.push(cut);
                        }
                    }
                    const cutHigh = createCut$2("min", varIndex, Math.ceil(varValue));
                    cutsHigh.push(cutHigh);
                    const cutLow = createCut$2("max", varIndex, Math.floor(varValue));
                    cutsLow.push(cutLow);
                    branches.push(createBranch$2(evaluation, cutsHigh));
                    branches.push(createBranch$2(evaluation, cutsLow));
                }
            }
            if (bestBranch !== null) {
                applyCuts(tableau, bestBranch.cuts);
            }
            tableau.branchAndCutIterations = iterations;
        };
        return { applyCuts, branchAndCut };
    }

    /**
     * Optimized cycle detector using hash-based O(1) lookup.
     * The original checkForCycles was O(n²) - comparing every pair against all others.
     * This version uses a Map to track where each (leaving,entering) pair occurred,
     * making duplicate detection O(1) average case.
     */
    class CycleDetector {
        constructor() {
            this.pairs = [];
            this.positions = new Map();
        }
        add(leaving, entering) {
            const key = `${leaving}_${entering}`;
            const pos = this.pairs.length;
            this.pairs.push([leaving, entering]);
            const prevPositions = this.positions.get(key);
            if (prevPositions === undefined) {
                this.positions.set(key, [pos]);
                return [];
            }
            // Check if any previous occurrence starts a repeating cycle
            for (const startPos of prevPositions) {
                const cycleLength = pos - startPos;
                // Need at least cycleLength more elements to verify
                if (cycleLength > this.pairs.length - pos) {
                    continue;
                }
                let cycleFound = true;
                for (let i = 1; i < cycleLength && startPos + cycleLength + i < this.pairs.length; i++) {
                    const p1 = this.pairs[startPos + i];
                    const p2 = this.pairs[startPos + cycleLength + i];
                    if (p1[0] !== p2[0] || p1[1] !== p2[1]) {
                        cycleFound = false;
                        break;
                    }
                }
                if (cycleFound) {
                    return [startPos, cycleLength];
                }
            }
            prevPositions.push(pos);
            return [];
        }
    }
    function simplex() {
        this.bounded = true;
        this.phase1();
        if (this.feasible === true) {
            this.phase2();
        }
        return this;
    }
    /**
     * Dual simplex algorithm for warm-starting after adding constraints.
     *
     * Use when: The current solution is dual feasible (reduced costs valid) but
     * may be primal infeasible (some RHS values negative). This is common after
     * adding bound constraints in branch-and-cut.
     *
     * Algorithm:
     * 1. Find a basic variable with negative value (leaving variable)
     * 2. Find entering variable using dual ratio test
     * 3. Pivot to restore primal feasibility
     * 4. Repeat until all basic variables are non-negative
     *
     * @returns Number of iterations, or -1 if dual infeasible
     */
    function dualSimplex() {
        const matrix = this.matrix;
        const width = this.width;
        const rhsColumn = this.rhsColumn;
        const lastColumn = width - 1;
        const lastRow = this.height - 1;
        const precision = this.precision;
        const negPrecision = -precision;
        let iterations = 0;
        const maxIterations = 10000; // Safety limit
        while (iterations < maxIterations) {
            // Step 1: Find leaving variable (row with most negative RHS)
            let leavingRow = 0;
            let minRHS = negPrecision;
            for (let r = 1; r <= lastRow; r++) {
                const rhsValue = matrix[r * width + rhsColumn];
                if (rhsValue < minRHS) {
                    minRHS = rhsValue;
                    leavingRow = r;
                }
            }
            // If no negative RHS, we're primal feasible - done!
            if (leavingRow === 0) {
                this.feasible = true;
                this.setEvaluation();
                return iterations;
            }
            // Step 2: Find entering variable using dual ratio test
            // For each non-basic variable j with a_ij < 0 (negative coefficient in leaving row),
            // compute ratio = reduced_cost[j] / |a_ij|
            // Choose the one with minimum ratio (to maintain dual feasibility)
            let enteringColumn = 0;
            let minRatio = Infinity;
            const leavingRowOffset = leavingRow * width;
            for (let c = 1; c <= lastColumn; c++) {
                const coefficient = matrix[leavingRowOffset + c];
                // Only consider columns with negative coefficient in leaving row
                if (coefficient < negPrecision) {
                    // Reduced cost is in row 0 (cost row)
                    const reducedCost = matrix[c];
                    // For minimization, reduced costs should be >= 0 for optimality
                    // Ratio test: reducedCost / |coefficient|
                    if (reducedCost >= negPrecision) {
                        const ratio = reducedCost / -coefficient;
                        if (ratio < minRatio) {
                            minRatio = ratio;
                            enteringColumn = c;
                        }
                    }
                }
            }
            // If no entering column found, the problem is dual infeasible (primal unbounded)
            if (enteringColumn === 0) {
                this.feasible = false;
                return -1;
            }
            // Step 3: Pivot
            this.pivot(leavingRow, enteringColumn);
            iterations++;
        }
        // Hit iteration limit - something went wrong
        this.feasible = false;
        return iterations;
    }
    function phase1() {
        const debugCheckForCycles = this.model.checkForCycles;
        const cycleDetector = debugCheckForCycles ? new CycleDetector() : null;
        const matrix = this.matrix;
        const width = this.width;
        const rhsColumn = this.rhsColumn;
        const lastColumn = this.width - 1;
        const lastRow = this.height - 1;
        const precision = this.precision;
        const negPrecision = -precision;
        // Cache arrays for faster access in hot loops
        const unrestrictedVars = this.unrestrictedVars;
        const varIndexByRow = this.varIndexByRow;
        const varIndexByCol = this.varIndexByCol;
        // Anti-cycling: on degenerate problems, the max-quotient pivot rule
        // can oscillate, corrupting the matrix. We detect stalling early
        // (after SAVE_THRESHOLD iterations without RHS improvement) and save
        // the matrix state. If still stuck after maxQuotientLimit iterations,
        // we restore the clean matrix and switch to Bland's rule. The lazy
        // save avoids the copy cost on the vast majority of phase1 calls
        // that converge quickly.
        const SAVE_THRESHOLD = 10;
        const maxQuotientLimit = Math.max(lastRow, lastColumn);
        let iterations = 0;
        let useBland = false;
        let initialRHS = -Infinity;
        let savedMatrix = null;
        let savedVarIndexByRow = null;
        let savedVarIndexByCol = null;
        let savedRowByVarIndex = null;
        let savedColByVarIndex = null;
        while (true) {
            // Find leaving row (most negative RHS among restricted basic vars).
            // Unrestricted variables can validly have negative values, so rows
            // where the basic variable is unrestricted are not infeasible.
            let leavingRowIndex = 0;
            let rhsValue = negPrecision;
            for (let r = 1; r <= lastRow; r++) {
                if (unrestrictedVars[varIndexByRow[r]] === true)
                    continue;
                const value = matrix[r * width + rhsColumn];
                if (value < rhsValue) {
                    rhsValue = value;
                    leavingRowIndex = r;
                }
            }
            if (leavingRowIndex === 0) {
                this.feasible = true;
                return iterations;
            }
            // Detect non-convergence and apply anti-cycling.
            if (!useBland && iterations > 0 && rhsValue <= initialRHS) {
                if (iterations >= SAVE_THRESHOLD && savedMatrix === null) {
                    // Stalling detected: save matrix for potential rollback
                    savedMatrix = matrix.slice();
                    savedVarIndexByRow = varIndexByRow.slice();
                    savedVarIndexByCol = varIndexByCol.slice();
                    savedRowByVarIndex = this.rowByVarIndex.slice();
                    savedColByVarIndex = this.colByVarIndex.slice();
                }
                if (iterations >= maxQuotientLimit) {
                    // Still stuck after generous budget: restore and switch
                    useBland = true;
                    if (savedMatrix) {
                        matrix.set(savedMatrix);
                        for (let i = 0; i < savedVarIndexByRow.length; i++) {
                            varIndexByRow[i] = savedVarIndexByRow[i];
                        }
                        for (let i = 0; i < savedVarIndexByCol.length; i++) {
                            varIndexByCol[i] = savedVarIndexByCol[i];
                        }
                        for (let i = 0; i < savedRowByVarIndex.length; i++) {
                            this.rowByVarIndex[i] = savedRowByVarIndex[i];
                        }
                        for (let i = 0; i < savedColByVarIndex.length; i++) {
                            this.colByVarIndex[i] = savedColByVarIndex[i];
                        }
                        iterations = 0;
                        continue;
                    }
                }
            }
            if (iterations === 0) {
                initialRHS = rhsValue;
            }
            // Find entering column.
            // Prefer columns with negative coefficient in leaving row (these directly
            // fix the infeasibility by making RHS positive after pivot).
            // Only fall back to unrestricted variables with positive coefficient when
            // no negative coefficient column exists - this "swaps" the infeasibility
            // to an unrestricted variable (which is allowed to be negative).
            let enteringColumn = 0;
            const leavingRowOffset = leavingRowIndex * width;
            if (useBland) {
                // Bland's rule: pick the first eligible column (smallest index).
                // First pass: negative coefficients only (directly fix infeasibility)
                for (let c = 1; c <= lastColumn; c++) {
                    const coefficient = matrix[leavingRowOffset + c];
                    if (coefficient < negPrecision) {
                        enteringColumn = c;
                        break;
                    }
                }
                // Fallback: unrestricted with non-zero coefficient (swap infeasibility)
                if (enteringColumn === 0) {
                    for (let c = 1; c <= lastColumn; c++) {
                        const coefficient = matrix[leavingRowOffset + c];
                        if (unrestrictedVars[varIndexByCol[c]] === true &&
                            (coefficient < negPrecision || coefficient > precision)) {
                            enteringColumn = c;
                            break;
                        }
                    }
                }
            }
            else {
                // Max-quotient rule: faster in practice but can oscillate on
                // degenerate problems.
                // First pass: negative coefficients only
                let maxQuotient = -Infinity;
                for (let c = 1; c <= lastColumn; c++) {
                    const coefficient = matrix[leavingRowOffset + c];
                    if (coefficient < negPrecision) {
                        const quotient = -matrix[c] / coefficient;
                        if (maxQuotient < quotient) {
                            maxQuotient = quotient;
                            enteringColumn = c;
                        }
                    }
                }
                // Fallback: unrestricted with non-zero coefficient
                if (enteringColumn === 0) {
                    for (let c = 1; c <= lastColumn; c++) {
                        const coefficient = matrix[leavingRowOffset + c];
                        if (unrestrictedVars[varIndexByCol[c]] === true &&
                            (coefficient < negPrecision || coefficient > precision)) {
                            enteringColumn = c;
                            break;
                        }
                    }
                }
            }
            if (enteringColumn === 0) {
                this.feasible = false;
                return iterations;
            }
            if (cycleDetector) {
                const cycleData = cycleDetector.add(varIndexByRow[leavingRowIndex], varIndexByCol[enteringColumn]);
                if (cycleData.length > 0) {
                    this.model.messages.push("Cycle in phase 1");
                    this.model.messages.push("Start :" + cycleData[0]);
                    this.model.messages.push("Length :" + cycleData[1]);
                    this.feasible = false;
                    return iterations;
                }
            }
            this.pivot(leavingRowIndex, enteringColumn);
            iterations += 1;
        }
    }
    function phase2() {
        const debugCheckForCycles = this.model.checkForCycles;
        const cycleDetector = debugCheckForCycles ? new CycleDetector() : null;
        const matrix = this.matrix;
        const width = this.width;
        const rhsColumn = this.rhsColumn;
        const lastColumn = this.width - 1;
        const lastRow = this.height - 1;
        const precision = this.precision;
        const negPrecision = -precision;
        const nOptionalObjectives = this.optionalObjectives.length;
        let optionalCostsColumns = null;
        // Cache arrays for faster access in hot loops
        const unrestrictedVars = this.unrestrictedVars;
        const varIndexByCol = this.varIndexByCol;
        const varIndexByRow = this.varIndexByRow;
        // Note: costRowIndex is always 0, so we access matrix[c] directly
        let iterations = 0;
        let reducedCost;
        let unrestricted;
        // Anti-cycling for phase 2: if the objective stalls (no meaningful
        // improvement over several hundred iterations), the simplex is
        // cycling through degenerate vertices. Switch to Bland's rule which
        // guarantees termination in exact arithmetic. In floating-point,
        // even Bland's can cycle indefinitely, so we also impose a limit:
        // if Bland's runs for lastRow iterations without improving the
        // objective, we accept the current solution as optimal.
        const PHASE2_WINDOW = 100;
        const PHASE2_STALE_LIMIT = 5; // consecutive stale windows before switching
        let useBland = false;
        let windowStartObj = matrix[rhsColumn];
        let staleWindows = 0;
        let blandStartIter = 0;
        let blandStartObj = 0;
        // Partial pricing setup
        // Batch size: use configured value or auto-compute (sqrt of columns, min 50, max 500)
        const nColumns = lastColumn;
        const batchSize = this.pricingBatchSize > 0
            ? this.pricingBatchSize
            : Math.min(500, Math.max(50, Math.floor(Math.sqrt(nColumns))));
        // For small problems, just scan everything (no benefit from partial pricing)
        const usePartialPricing = nColumns > batchSize * 2;
        while (true) {
            if (nOptionalObjectives > 0) {
                optionalCostsColumns = [];
            }
            // Detect degenerate cycling: check every WINDOW iterations
            // whether the objective has made meaningful progress.
            if (!useBland && iterations > 0 && iterations % PHASE2_WINDOW === 0) {
                const currentObj = matrix[rhsColumn];
                const delta = Math.abs(currentObj - windowStartObj);
                const scale = Math.max(1, Math.abs(windowStartObj));
                if (delta / scale < 1e-10) {
                    staleWindows++;
                    if (staleWindows >= PHASE2_STALE_LIMIT) {
                        useBland = true;
                        blandStartIter = iterations;
                        blandStartObj = currentObj;
                    }
                }
                else {
                    staleWindows = 0;
                }
                windowStartObj = currentObj;
            }
            // Bland's termination: in floating-point arithmetic, Bland's rule
            // can cycle through degenerate bases indefinitely. If it hasn't
            // improved the objective after lastRow pivots, accept the current
            // solution (reduced costs are within floating-point noise of zero).
            if (useBland && iterations - blandStartIter > lastRow) {
                const currentObj = matrix[rhsColumn];
                const delta = Math.abs(currentObj - blandStartObj);
                const scale = Math.max(1, Math.abs(blandStartObj));
                if (delta / scale < 1e-10) {
                    this.setEvaluation();
                    this.simplexIters += 1;
                    return iterations;
                }
                // Objective did improve; reset the counter
                blandStartIter = iterations;
                blandStartObj = currentObj;
            }
            let enteringColumn = 0;
            let enteringValue = precision;
            let isReducedCostNegative = false;
            if (useBland) {
                // Bland's rule: pick first eligible column (smallest index)
                for (let c = 1; c <= lastColumn; c++) {
                    reducedCost = matrix[c];
                    unrestricted = unrestrictedVars[varIndexByCol[c]] === true;
                    if (unrestricted && reducedCost < 0) {
                        enteringColumn = c;
                        enteringValue = -reducedCost;
                        isReducedCostNegative = true;
                        break;
                    }
                    if (reducedCost > precision) {
                        enteringColumn = c;
                        enteringValue = reducedCost;
                        isReducedCostNegative = false;
                        break;
                    }
                }
            }
            else if (usePartialPricing) {
                // Partial pricing: scan columns in batches
                const startBatch = this.pricingBatchStart;
                let batchesScanned = 0;
                const totalBatches = Math.ceil(nColumns / batchSize);
                // Scan batches until we find an improving column or exhaust all batches
                while (enteringColumn === 0 && batchesScanned < totalBatches) {
                    const batchStart = this.pricingBatchStart;
                    const batchEnd = Math.min(batchStart + batchSize - 1, lastColumn);
                    for (let c = batchStart; c <= batchEnd; c++) {
                        reducedCost = matrix[c]; // costRowOffset is 0
                        unrestricted = unrestrictedVars[varIndexByCol[c]] === true;
                        if (nOptionalObjectives > 0 &&
                            negPrecision < reducedCost &&
                            reducedCost < precision) {
                            optionalCostsColumns === null || optionalCostsColumns === void 0 ? void 0 : optionalCostsColumns.push(c);
                            continue;
                        }
                        if (unrestricted && reducedCost < 0) {
                            if (-reducedCost > enteringValue) {
                                enteringValue = -reducedCost;
                                enteringColumn = c;
                                isReducedCostNegative = true;
                            }
                            continue;
                        }
                        if (reducedCost > enteringValue) {
                            enteringValue = reducedCost;
                            enteringColumn = c;
                            isReducedCostNegative = false;
                        }
                    }
                    // Move to next batch (wrap around)
                    this.pricingBatchStart = batchEnd >= lastColumn ? 1 : batchEnd + 1;
                    batchesScanned++;
                }
                // Reset batch start if we found an improving column
                if (enteringColumn !== 0) {
                    this.pricingBatchStart = startBatch;
                }
            }
            else {
                // Full pricing for small problems
                for (let c = 1; c <= lastColumn; c++) {
                    reducedCost = matrix[c]; // costRowOffset is 0
                    unrestricted = unrestrictedVars[varIndexByCol[c]] === true;
                    if (nOptionalObjectives > 0 &&
                        negPrecision < reducedCost &&
                        reducedCost < precision) {
                        optionalCostsColumns === null || optionalCostsColumns === void 0 ? void 0 : optionalCostsColumns.push(c);
                        continue;
                    }
                    if (unrestricted && reducedCost < 0) {
                        if (-reducedCost > enteringValue) {
                            enteringValue = -reducedCost;
                            enteringColumn = c;
                            isReducedCostNegative = true;
                        }
                        continue;
                    }
                    if (reducedCost > enteringValue) {
                        enteringValue = reducedCost;
                        enteringColumn = c;
                        isReducedCostNegative = false;
                    }
                }
            }
            if (nOptionalObjectives > 0) {
                let o = 0;
                while (enteringColumn === 0 &&
                    optionalCostsColumns &&
                    optionalCostsColumns.length > 0 &&
                    o < nOptionalObjectives) {
                    const optionalCostsColumns2 = [];
                    const reducedCosts = this.optionalObjectives[o].reducedCosts;
                    enteringValue = precision;
                    for (let i = 0; i < optionalCostsColumns.length; i++) {
                        const c = optionalCostsColumns[i];
                        reducedCost = reducedCosts[c];
                        unrestricted = unrestrictedVars[varIndexByCol[c]] === true;
                        if (negPrecision < reducedCost && reducedCost < precision) {
                            optionalCostsColumns2.push(c);
                            continue;
                        }
                        if (unrestricted && reducedCost < 0) {
                            if (-reducedCost > enteringValue) {
                                enteringValue = -reducedCost;
                                enteringColumn = c;
                                isReducedCostNegative = true;
                            }
                            continue;
                        }
                        if (reducedCost > enteringValue) {
                            enteringValue = reducedCost;
                            enteringColumn = c;
                            isReducedCostNegative = false;
                        }
                    }
                    optionalCostsColumns = optionalCostsColumns2;
                    o += 1;
                }
            }
            if (enteringColumn === 0) {
                this.setEvaluation();
                this.simplexIters += 1;
                return iterations;
            }
            let leavingRow = 0;
            let minQuotient = Infinity;
            for (let r = 1; r <= lastRow; r++) {
                const rowOffset = r * width;
                const rhsValue = matrix[rowOffset + rhsColumn];
                const colValue = matrix[rowOffset + enteringColumn];
                if (negPrecision < colValue && colValue < precision) {
                    continue;
                }
                if (colValue > 0 && precision > rhsValue && rhsValue > negPrecision) {
                    minQuotient = 0;
                    leavingRow = r;
                    break;
                }
                const quotient = isReducedCostNegative ? -rhsValue / colValue : rhsValue / colValue;
                if (quotient > precision && minQuotient > quotient) {
                    minQuotient = quotient;
                    leavingRow = r;
                }
            }
            if (minQuotient === Infinity) {
                this.evaluation = -Infinity;
                this.bounded = false;
                this.unboundedVarIndex = varIndexByCol[enteringColumn];
                return iterations;
            }
            if (cycleDetector) {
                const cycleData = cycleDetector.add(varIndexByRow[leavingRow], varIndexByCol[enteringColumn]);
                if (cycleData.length > 0) {
                    this.model.messages.push("Cycle in phase 2");
                    this.model.messages.push("Start :" + cycleData[0]);
                    this.model.messages.push("Length :" + cycleData[1]);
                    this.feasible = false;
                    return iterations;
                }
            }
            this.pivot(leavingRow, enteringColumn);
            iterations += 1;
        }
    }
    // Pre-allocated typed arrays for pivot optimization (better cache performance)
    let nonZeroColumns = new Int32Array(1024);
    let pivotRowCache = new Float64Array(1024);
    function pivot(pivotRowIndex, pivotColumnIndex) {
        const matrix = this.matrix;
        const width = this.width;
        // Ensure work arrays are large enough
        if (width > nonZeroColumns.length) {
            nonZeroColumns = new Int32Array(width * 2);
            pivotRowCache = new Float64Array(width * 2);
        }
        const pivotRowOffset = pivotRowIndex * width;
        const quotient = matrix[pivotRowOffset + pivotColumnIndex];
        const invQuotient = 1 / quotient;
        const height = this.height;
        const leavingBasicIndex = this.varIndexByRow[pivotRowIndex];
        const enteringBasicIndex = this.varIndexByCol[pivotColumnIndex];
        this.varIndexByRow[pivotRowIndex] = enteringBasicIndex;
        this.varIndexByCol[pivotColumnIndex] = leavingBasicIndex;
        this.rowByVarIndex[enteringBasicIndex] = pivotRowIndex;
        this.rowByVarIndex[leavingBasicIndex] = -1;
        this.colByVarIndex[enteringBasicIndex] = -1;
        this.colByVarIndex[leavingBasicIndex] = pivotColumnIndex;
        // Normalize pivot row, track non-zero columns, and cache values for locality
        let nNonZeroColumns = 0;
        for (let c = 0; c < width; c++) {
            const idx = pivotRowOffset + c;
            const val = matrix[idx];
            if (!(val >= -1e-16 && val <= 1e-16)) {
                const normalized = val / quotient;
                matrix[idx] = normalized;
                nonZeroColumns[nNonZeroColumns] = c;
                pivotRowCache[nNonZeroColumns] = normalized;
                nNonZeroColumns++;
            }
            else {
                matrix[idx] = 0;
            }
        }
        matrix[pivotRowOffset + pivotColumnIndex] = invQuotient;
        // Update all other rows using cached pivot row values
        for (let r = 0; r < height; r++) {
            if (r !== pivotRowIndex) {
                const rowOffset = r * width;
                const pivotColVal = matrix[rowOffset + pivotColumnIndex];
                if (!(pivotColVal >= -1e-16 && pivotColVal <= 1e-16)) {
                    const coefficient = pivotColVal;
                    if (!(coefficient >= -1e-16 && coefficient <= 1e-16)) {
                        // Use cached pivot row values for better cache locality
                        for (let i = 0; i < nNonZeroColumns; i++) {
                            const c = nonZeroColumns[i];
                            const v0 = pivotRowCache[i];
                            // Inner zero check is critical for numerical stability
                            if (!(v0 >= -1e-16 && v0 <= 1e-16)) {
                                matrix[rowOffset + c] -= coefficient * v0;
                            }
                            else if (v0 !== 0) {
                                // Clean up near-zero values in pivot row
                                matrix[pivotRowOffset + c] = 0;
                            }
                        }
                        matrix[rowOffset + pivotColumnIndex] = -coefficient / quotient;
                    }
                    else if (coefficient !== 0) {
                        matrix[rowOffset + pivotColumnIndex] = 0;
                    }
                }
            }
        }
        // Update optional objectives using cached pivot row values
        const optionalObjectives = this.optionalObjectives;
        const nOptionalObjectives = optionalObjectives.length;
        if (nOptionalObjectives > 0) {
            for (let o = 0; o < nOptionalObjectives; o++) {
                const reducedCosts = optionalObjectives[o].reducedCosts;
                const coefficient = reducedCosts[pivotColumnIndex];
                if (coefficient !== 0) {
                    for (let i = 0; i < nNonZeroColumns; i++) {
                        const c = nonZeroColumns[i];
                        reducedCosts[c] -= coefficient * pivotRowCache[i];
                    }
                    reducedCosts[pivotColumnIndex] = -coefficient * invQuotient;
                }
            }
        }
    }
    function checkForCycles(varIndexes) {
        for (let e1 = 0; e1 < varIndexes.length - 1; e1++) {
            for (let e2 = e1 + 1; e2 < varIndexes.length; e2++) {
                const elt1 = varIndexes[e1];
                const elt2 = varIndexes[e2];
                if (elt1[0] === elt2[0] && elt1[1] === elt2[1]) {
                    if (e2 - e1 > varIndexes.length - e2) {
                        break;
                    }
                    let cycleFound = true;
                    for (let i = 1; i < e2 - e1; i++) {
                        const tmp1 = varIndexes[e1 + i];
                        const tmp2 = varIndexes[e2 + i];
                        if (tmp1[0] !== tmp2[0] || tmp1[1] !== tmp2[1]) {
                            cycleFound = false;
                            break;
                        }
                    }
                    if (cycleFound) {
                        return [e1, e2 - e1];
                    }
                }
            }
        }
        return [];
    }

    class Variable {
        constructor(id, cost, index, priority) {
            this.id = id;
            this.cost = cost;
            this.index = index;
            this.value = 0;
            this.priority = priority;
        }
    }
    class IntegerVariable extends Variable {
        constructor(id, cost, index, priority) {
            super(id, cost, index, priority);
            this.isInteger = true;
        }
    }
    class SlackVariable extends Variable {
        constructor(id, index) {
            super(id, 0, index, 0);
            this.isSlack = true;
        }
    }
    class Term {
        constructor(variable, coefficient) {
            this.variable = variable;
            this.coefficient = coefficient;
        }
    }
    function createRelaxationVariable(model, weight, priority) {
        if (priority === 0 || priority === "required") {
            return null;
        }
        const normalizedWeight = weight === undefined ? 1 : weight;
        const normalizedPriority = priority === undefined ? 1 : priority;
        const actualWeight = model.isMinimization === false ? -normalizedWeight : normalizedWeight;
        return model.addVariable(actualWeight, "r" + model.relaxationIndex++, false, false, normalizedPriority);
    }
    class Constraint {
        constructor(rhs, isUpperBound, index, model) {
            this.slack = new SlackVariable("s" + index, index);
            this.index = index;
            this.model = model;
            this.rhs = rhs;
            this.isUpperBound = isUpperBound;
            this.terms = [];
            this.termsByVarIndex = {};
            this.relaxation = null;
        }
        addTerm(coefficient, variable) {
            const varIndex = variable.index;
            const term = this.termsByVarIndex[varIndex];
            if (term === undefined) {
                // No term for given variable
                const newTerm = new Term(variable, coefficient);
                this.termsByVarIndex[varIndex] = newTerm;
                this.terms.push(newTerm);
                const signedCoefficient = this.isUpperBound === true ? -coefficient : coefficient;
                this.model.updateConstraintCoefficient(this, variable, signedCoefficient);
            }
            else {
                // Term for given variable already exists
                // updating its coefficient
                const newCoefficient = term.coefficient + coefficient;
                this.setVariableCoefficient(newCoefficient, variable);
            }
            return this;
        }
        // TODO: Implement term removal if required by consumers.
        removeTerm(_term) {
            return this;
        }
        setRightHandSide(newRhs) {
            if (newRhs !== this.rhs) {
                let difference = newRhs - this.rhs;
                if (this.isUpperBound === true) {
                    difference = -difference;
                }
                this.rhs = newRhs;
                this.model.updateRightHandSide(this, difference);
            }
            return this;
        }
        setVariableCoefficient(newCoefficient, variable) {
            const varIndex = variable.index;
            if (varIndex === -1) {
                // eslint-disable-next-line no-console
                console.warn("[Constraint.setVariableCoefficient] Trying to change coefficient of inexistant variable.");
                return;
            }
            const term = this.termsByVarIndex[varIndex];
            if (term === undefined) {
                // No term for given variable
                this.addTerm(newCoefficient, variable);
            }
            else if (newCoefficient !== term.coefficient) {
                // Term for given variable already exists
                // updating its coefficient if changed
                let difference = newCoefficient - term.coefficient;
                if (this.isUpperBound === true) {
                    difference = -difference;
                }
                term.coefficient = newCoefficient;
                this.model.updateConstraintCoefficient(this, variable, difference);
            }
            return this;
        }
        relax(weight, priority) {
            this.relaxation = createRelaxationVariable(this.model, weight, priority);
            this._relax(this.relaxation);
        }
        _relax(relaxationVariable) {
            if (relaxationVariable === null) {
                // Relaxation variable not created, priority was probably "required"
                return;
            }
            if (this.isUpperBound) {
                this.setVariableCoefficient(-1, relaxationVariable);
            }
            else {
                this.setVariableCoefficient(1, relaxationVariable);
            }
        }
    }
    class Equality {
        constructor(constraintUpper, constraintLower) {
            this.isEquality = true;
            this.upperBound = constraintUpper;
            this.lowerBound = constraintLower;
            this.model = constraintUpper.model;
            this.rhs = constraintUpper.rhs;
            this.relaxation = null;
        }
        addTerm(coefficient, variable) {
            this.upperBound.addTerm(coefficient, variable);
            this.lowerBound.addTerm(coefficient, variable);
            return this;
        }
        // TODO: Implement term removal if required by consumers.
        removeTerm(_term) {
            this.upperBound.removeTerm(_term);
            this.lowerBound.removeTerm(_term);
            return this;
        }
        setRightHandSide(rhs) {
            this.upperBound.setRightHandSide(rhs);
            this.lowerBound.setRightHandSide(rhs);
            this.rhs = rhs;
        }
        relax(weight, priority) {
            this.relaxation = createRelaxationVariable(this.model, weight, priority);
            this.upperBound.relaxation = this.relaxation;
            this.upperBound._relax(this.relaxation);
            this.lowerBound.relaxation = this.relaxation;
            this.lowerBound._relax(this.relaxation);
        }
    }
    class Numeral {
        constructor(value) {
            this.value = value;
        }
    }

    function addCutConstraints(cutConstraints) {
        const nCutConstraints = cutConstraints.length;
        const height = this.height;
        const heightWithCuts = height + nCutConstraints;
        const width = this.width;
        const lastColumn = width - 1;
        // Grow the matrix to accommodate new rows (with over-allocation to reduce reallocation frequency)
        const oldMatrix = this.matrix;
        const newSize = heightWithCuts * width;
        if (oldMatrix.length < newSize) {
            // Over-allocate by 50% to reduce future reallocations
            const allocSize = Math.ceil(newSize * 1.5);
            const newMatrix = new Float64Array(allocSize);
            newMatrix.set(oldMatrix);
            this.matrix = newMatrix;
        }
        const matrix = this.matrix;
        this.height = heightWithCuts;
        this.nVars = this.width + this.height - 2;
        // Cache array references for faster access in loop
        const rhsColumn = this.rhsColumn;
        const rowByVarIndex = this.rowByVarIndex;
        const colByVarIndex = this.colByVarIndex;
        const varIndexByRow = this.varIndexByRow;
        const variablesPerIndex = this.variablesPerIndex;
        for (let h = 0; h < nCutConstraints; h += 1) {
            const cut = cutConstraints[h];
            const cutRow = height + h;
            const cutRowOffset = cutRow * width;
            const sign = cut.type === "min" ? -1 : 1;
            const varIndex = cut.varIndex;
            let varRowIndex = rowByVarIndex[varIndex];
            if (varRowIndex === -1) {
                matrix[cutRowOffset + rhsColumn] = sign * cut.value;
                for (let c = 1; c <= lastColumn; c += 1) {
                    matrix[cutRowOffset + c] = 0;
                }
                matrix[cutRowOffset + colByVarIndex[varIndex]] = sign;
            }
            else {
                const varRowOffset = varRowIndex * width;
                const varValue = matrix[varRowOffset + rhsColumn];
                matrix[cutRowOffset + rhsColumn] = sign * (cut.value - varValue);
                for (let c = 1; c <= lastColumn; c += 1) {
                    matrix[cutRowOffset + c] = -sign * matrix[varRowOffset + c];
                }
            }
            varRowIndex = this.getNewElementIndex();
            varIndexByRow[cutRow] = varRowIndex;
            rowByVarIndex[varRowIndex] = cutRow;
            colByVarIndex[varRowIndex] = -1;
            variablesPerIndex[varRowIndex] = new SlackVariable("s" + varRowIndex, varRowIndex);
            this.nVars += 1;
        }
    }
    function addLowerBoundMIRCut(rowIndex) {
        if (rowIndex === this.costRowIndex) {
            return false;
        }
        const width = this.width;
        const matrix = this.matrix;
        const cutRowOffset = rowIndex * width;
        const integerVar = this.variablesPerIndex[this.varIndexByRow[rowIndex]];
        if (integerVar === undefined || !integerVar.isInteger) {
            return false;
        }
        const rhsValue = matrix[cutRowOffset + this.rhsColumn];
        const fractionalPart = rhsValue - Math.floor(rhsValue);
        if (fractionalPart < this.precision || fractionalPart > 1 - this.precision) {
            return false;
        }
        const height = this.height;
        const newRowOffset = height * width;
        // Grow matrix to add new row (with over-allocation to reduce reallocation frequency)
        const newSize = (height + 1) * width;
        if (matrix.length < newSize) {
            // Over-allocate by 50% to reduce future reallocations
            const allocSize = Math.ceil(newSize * 1.5);
            const newMatrix = new Float64Array(allocSize);
            newMatrix.set(matrix);
            this.matrix = newMatrix;
        }
        const mat = this.matrix;
        this.height += 1;
        this.nVars += 1;
        const slackVarIndex = this.getNewElementIndex();
        this.varIndexByRow[height] = slackVarIndex;
        this.rowByVarIndex[slackVarIndex] = height;
        this.colByVarIndex[slackVarIndex] = -1;
        this.variablesPerIndex[slackVarIndex] = new SlackVariable("s" + slackVarIndex, slackVarIndex);
        const rhsColumn = this.rhsColumn;
        mat[newRowOffset + rhsColumn] = Math.floor(rhsValue);
        // Cache array references for faster access in hot loop
        const variablesPerIndex = this.variablesPerIndex;
        const varIndexByCol = this.varIndexByCol;
        const varIndexByColLen = varIndexByCol.length;
        const oneMinusFrac = 1 - fractionalPart;
        for (let colIndex = 1; colIndex < varIndexByColLen; colIndex += 1) {
            const variable = variablesPerIndex[varIndexByCol[colIndex]];
            const coefficient = mat[cutRowOffset + colIndex];
            if (variable !== undefined && variable.isInteger) {
                const floorCoeff = Math.floor(coefficient);
                const termCoeff = floorCoeff + Math.max(0, coefficient - floorCoeff - fractionalPart) / oneMinusFrac;
                mat[newRowOffset + colIndex] = termCoeff;
            }
            else {
                mat[newRowOffset + colIndex] = Math.min(0, coefficient / oneMinusFrac);
            }
        }
        for (let c = 0; c < width; c += 1) {
            mat[newRowOffset + c] -= mat[cutRowOffset + c];
        }
        return true;
    }
    function addUpperBoundMIRCut(rowIndex) {
        if (rowIndex === this.costRowIndex) {
            return false;
        }
        const width = this.width;
        const matrix = this.matrix;
        const cutRowOffset = rowIndex * width;
        const integerVar = this.variablesPerIndex[this.varIndexByRow[rowIndex]];
        if (integerVar === undefined || !integerVar.isInteger) {
            return false;
        }
        const rhsValue = matrix[cutRowOffset + this.rhsColumn];
        const fractionalPart = rhsValue - Math.floor(rhsValue);
        if (fractionalPart < this.precision || fractionalPart > 1 - this.precision) {
            return false;
        }
        const height = this.height;
        const newRowOffset = height * width;
        // Grow matrix to add new row (with over-allocation to reduce reallocation frequency)
        const newSize = (height + 1) * width;
        if (matrix.length < newSize) {
            // Over-allocate by 50% to reduce future reallocations
            const allocSize = Math.ceil(newSize * 1.5);
            const newMatrix = new Float64Array(allocSize);
            newMatrix.set(matrix);
            this.matrix = newMatrix;
        }
        const mat = this.matrix;
        this.height += 1;
        this.nVars += 1;
        const slackVarIndex = this.getNewElementIndex();
        this.varIndexByRow[height] = slackVarIndex;
        this.rowByVarIndex[slackVarIndex] = height;
        this.colByVarIndex[slackVarIndex] = -1;
        this.variablesPerIndex[slackVarIndex] = new SlackVariable("s" + slackVarIndex, slackVarIndex);
        const rhsColumn = this.rhsColumn;
        mat[newRowOffset + rhsColumn] = -fractionalPart;
        // Cache array references for faster access in hot loop
        const variablesPerIndex = this.variablesPerIndex;
        const varIndexByCol = this.varIndexByCol;
        const varIndexByColLen = varIndexByCol.length;
        const oneMinusFrac = 1 - fractionalPart;
        for (let colIndex = 1; colIndex < varIndexByColLen; colIndex += 1) {
            const variable = variablesPerIndex[varIndexByCol[colIndex]];
            const coefficient = mat[cutRowOffset + colIndex];
            const termCoeff = coefficient - Math.floor(coefficient);
            if (variable !== undefined && variable.isInteger) {
                mat[newRowOffset + colIndex] =
                    termCoeff <= fractionalPart
                        ? -termCoeff
                        : (-(1 - termCoeff) * fractionalPart) / termCoeff;
            }
            else {
                mat[newRowOffset + colIndex] =
                    coefficient >= 0 ? -coefficient : (coefficient * fractionalPart) / oneMinusFrac;
            }
        }
        return true;
    }
    function applyMIRCuts() {
        // Apply MIR (Mixed Integer Rounding) cuts to all rows with fractional integer variables
        // This tightens the LP relaxation and can help prune the branch-and-bound tree
        const height = this.height;
        let cutsAdded = 0;
        const maxCuts = 10; // Limit cuts per iteration to avoid excessive growth
        for (let r = 1; r < height && cutsAdded < maxCuts; r++) {
            // Try lower bound MIR cut first (typically more effective)
            if (this.addLowerBoundMIRCut(r)) {
                cutsAdded++;
            }
        }
    }

    function putInBase(varIndex) {
        const width = this.width;
        let r = this.rowByVarIndex[varIndex];
        if (r === -1) {
            const c = this.colByVarIndex[varIndex];
            for (let r1 = 1; r1 < this.height; r1 += 1) {
                const coefficient = this.matrix[r1 * width + c];
                if (coefficient < -this.precision || this.precision < coefficient) {
                    r = r1;
                    break;
                }
            }
            this.pivot(r, c);
        }
        return r;
    }
    function takeOutOfBase(varIndex) {
        const width = this.width;
        let c = this.colByVarIndex[varIndex];
        if (c === -1) {
            const r = this.rowByVarIndex[varIndex];
            const pivotRowOffset = r * width;
            for (let c1 = 1; c1 < this.height; c1 += 1) {
                const coefficient = this.matrix[pivotRowOffset + c1];
                if (coefficient < -this.precision || this.precision < coefficient) {
                    c = c1;
                    break;
                }
            }
            this.pivot(r, c);
        }
        return c;
    }
    function updateVariableValues() {
        const width = this.width;
        const matrix = this.matrix;
        const rhsColumn = this.rhsColumn;
        const nVars = this.variables.length;
        const roundingCoeff = Math.round(1 / this.precision);
        for (let v = 0; v < nVars; v += 1) {
            const variable = this.variables[v];
            const varIndex = variable.index;
            const r = this.rowByVarIndex[varIndex];
            if (r === -1) {
                variable.value = 0;
            }
            else {
                const varValue = matrix[r * width + rhsColumn];
                variable.value =
                    Math.round((varValue + Number.EPSILON) * roundingCoeff) / roundingCoeff;
            }
        }
    }
    function updateRightHandSide(constraint, difference) {
        const width = this.width;
        const matrix = this.matrix;
        const rhsColumn = this.rhsColumn;
        const lastRow = this.height - 1;
        const constraintRow = this.rowByVarIndex[constraint.index];
        if (constraintRow === -1) {
            const slackColumn = this.colByVarIndex[constraint.index];
            for (let r = 0; r <= lastRow; r += 1) {
                const rowOffset = r * width;
                matrix[rowOffset + rhsColumn] -= difference * matrix[rowOffset + slackColumn];
            }
            const nOptionalObjectives = this.optionalObjectives.length;
            if (nOptionalObjectives > 0) {
                for (let o = 0; o < nOptionalObjectives; o += 1) {
                    const reducedCosts = this.optionalObjectives[o].reducedCosts;
                    reducedCosts[rhsColumn] -= difference * reducedCosts[slackColumn];
                }
            }
        }
        else {
            matrix[constraintRow * width + rhsColumn] -= difference;
        }
    }
    function updateConstraintCoefficient(constraint, variable, difference) {
        if (constraint.index === variable.index) {
            throw new Error("[Tableau.updateConstraintCoefficient] constraint index should not be equal to variable index !");
        }
        const width = this.width;
        const matrix = this.matrix;
        const r = this.putInBase(constraint.index);
        const rowOffset = r * width;
        const colVar = this.colByVarIndex[variable.index];
        if (colVar === -1) {
            const rowVar = this.rowByVarIndex[variable.index];
            const rowVarOffset = rowVar * width;
            for (let c = 0; c < width; c += 1) {
                matrix[rowOffset + c] += difference * matrix[rowVarOffset + c];
            }
        }
        else {
            matrix[rowOffset + colVar] -= difference;
        }
    }
    function updateCost(variable, difference) {
        const width = this.width;
        const matrix = this.matrix;
        const varIndex = variable.index;
        const lastColumn = width - 1;
        const varColumn = this.colByVarIndex[varIndex];
        if (varColumn === -1) {
            const variableRowOffset = this.rowByVarIndex[varIndex] * width;
            if (variable.priority === 0) {
                // Cost row is row 0
                for (let c = 0; c <= lastColumn; c += 1) {
                    matrix[c] += difference * matrix[variableRowOffset + c];
                }
            }
            else {
                const reducedCosts = this.objectivesByPriority[variable.priority].reducedCosts;
                for (let c = 0; c <= lastColumn; c += 1) {
                    reducedCosts[c] += difference * matrix[variableRowOffset + c];
                }
            }
        }
        else {
            matrix[varColumn] -= difference; // row 0, col varColumn
        }
    }
    function addConstraint(constraint) {
        const sign = constraint.isUpperBound ? 1 : -1;
        const lastRow = this.height;
        const width = this.width;
        const lastColumn = width - 1;
        // Check if we need to grow the matrix capacity (using exponential growth)
        const newRowCount = lastRow + 1;
        const requiredSize = newRowCount * width;
        if (this.matrix.length < requiredSize) {
            // Use exponential growth strategy (1.5x) with minimum increment
            const currentCapacity = this.matrix.length;
            const minGrowth = Math.max(width * 16, Math.floor(currentCapacity * 0.5));
            const newCapacity = currentCapacity + minGrowth;
            const oldMatrix = this.matrix;
            const newMatrix = new Float64Array(newCapacity);
            newMatrix.set(oldMatrix);
            this.matrix = newMatrix;
        }
        const matrix = this.matrix;
        const constraintRowOffset = lastRow * width;
        // Zero out the new row
        for (let c = 0; c <= lastColumn; c += 1) {
            matrix[constraintRowOffset + c] = 0;
        }
        matrix[constraintRowOffset + this.rhsColumn] = sign * constraint.rhs;
        const terms = constraint.terms;
        const nTerms = terms.length;
        for (let t = 0; t < nTerms; t += 1) {
            const term = terms[t];
            const coefficient = term.coefficient;
            const varIndex = term.variable.index;
            const varRowIndex = this.rowByVarIndex[varIndex];
            if (varRowIndex === -1) {
                matrix[constraintRowOffset + this.colByVarIndex[varIndex]] += sign * coefficient;
            }
            else {
                const varRowOffset = varRowIndex * width;
                for (let c = 0; c <= lastColumn; c += 1) {
                    matrix[constraintRowOffset + c] -= sign * coefficient * matrix[varRowOffset + c];
                }
            }
        }
        const slackIndex = constraint.index;
        this.varIndexByRow[lastRow] = slackIndex;
        this.rowByVarIndex[slackIndex] = lastRow;
        this.colByVarIndex[slackIndex] = -1;
        this.height += 1;
    }
    function removeConstraint(constraint) {
        const slackIndex = constraint.index;
        const lastRow = this.height - 1;
        const width = this.width;
        const matrix = this.matrix;
        const r = this.putInBase(slackIndex);
        // Swap row r with lastRow
        const rowOffset = r * width;
        const lastRowOffset = lastRow * width;
        for (let c = 0; c < width; c++) {
            const tmp = matrix[lastRowOffset + c];
            matrix[lastRowOffset + c] = matrix[rowOffset + c];
            matrix[rowOffset + c] = tmp;
        }
        this.varIndexByRow[r] = this.varIndexByRow[lastRow];
        this.varIndexByRow[lastRow] = -1;
        this.rowByVarIndex[slackIndex] = -1;
        this.availableIndexes[this.availableIndexes.length] = slackIndex;
        constraint.slack.index = -1;
        this.height -= 1;
    }
    function addVariable(variable) {
        this.height - 1;
        const oldWidth = this.width;
        const newWidth = oldWidth + 1;
        const height = this.height;
        const cost = this.model.isMinimization === true ? -variable.cost : variable.cost;
        const priority = variable.priority;
        // Need to expand the matrix to add a new column
        // This requires reallocating and copying with new layout
        // Note: Column capacity optimization would require changing all matrix access
        // to use capacity as stride, which is too invasive. Keep simple reallocation.
        const oldMatrix = this.matrix;
        const newMatrix = new Float64Array(height * newWidth);
        // Copy old data with new width
        for (let r = 0; r < height; r++) {
            const oldOffset = r * oldWidth;
            const newOffset = r * newWidth;
            for (let c = 0; c < oldWidth; c++) {
                newMatrix[newOffset + c] = oldMatrix[oldOffset + c];
            }
            // New column is 0 by default
        }
        this.matrix = newMatrix;
        this.width = newWidth;
        const lastColumn = newWidth - 1;
        const nOptionalObjectives = this.optionalObjectives.length;
        if (nOptionalObjectives > 0) {
            for (let o = 0; o < nOptionalObjectives; o += 1) {
                this.optionalObjectives[o].reducedCosts[lastColumn] = 0;
            }
        }
        if (priority === 0) {
            newMatrix[lastColumn] = cost; // row 0, col lastColumn
        }
        else {
            this.setOptionalObjective(priority, lastColumn, cost);
            newMatrix[lastColumn] = 0;
        }
        this.colByVarIndex[variable.index] = lastColumn;
        this.varIndexByCol[lastColumn] = variable.index;
    }
    function removeVariable(variable) {
        const varIndex = variable.index;
        const width = this.width;
        const matrix = this.matrix;
        const lastColumn = width - 1;
        const c = this.takeOutOfBase(varIndex);
        const lastRow = this.height - 1;
        for (let r = 0; r <= lastRow; r += 1) {
            const rowOffset = r * width;
            matrix[rowOffset + c] = matrix[rowOffset + lastColumn];
        }
        this.varIndexByCol[c] = this.varIndexByCol[lastColumn];
        this.rowByVarIndex[varIndex] = -1;
        this.colByVarIndex[varIndex] = -1;
        this.availableIndexes[this.availableIndexes.length] = varIndex;
        this.width -= 1;
    }

    function copy() {
        const copy = new this.constructor(this.precision, this.branchAndCutService);
        copy.width = this.width;
        copy.height = this.height;
        copy.nVars = this.nVars;
        copy.model = this.model;
        copy.variables = this.variables;
        copy.variablesPerIndex = this.variablesPerIndex;
        copy.unrestrictedVars = this.unrestrictedVars;
        copy.lastElementIndex = this.lastElementIndex;
        copy.varIndexByRow = this.varIndexByRow.slice();
        copy.varIndexByCol = this.varIndexByCol.slice();
        copy.rowByVarIndex = this.rowByVarIndex.slice();
        copy.colByVarIndex = this.colByVarIndex.slice();
        copy.availableIndexes = this.availableIndexes.slice();
        const optionalObjectivesCopy = [];
        for (let o = 0; o < this.optionalObjectives.length; o++) {
            optionalObjectivesCopy[o] = this.optionalObjectives[o].copy();
        }
        copy.optionalObjectives = optionalObjectivesCopy;
        copy.objectivesByPriority = { ...this.objectivesByPriority };
        copy.optionalObjectivePerPriority = { ...this.optionalObjectivePerPriority };
        // Fast Float64Array copy using constructor
        copy.matrix = new Float64Array(this.matrix);
        return copy;
    }
    function save() {
        this.savedState = this.copy();
    }
    function restore() {
        if (this.savedState === null) {
            return;
        }
        const save = this.savedState;
        this.nVars = save.nVars;
        this.model = save.model;
        this.variables = save.variables;
        this.variablesPerIndex = save.variablesPerIndex;
        this.unrestrictedVars = save.unrestrictedVars;
        this.lastElementIndex = save.lastElementIndex;
        this.width = save.width;
        this.height = save.height;
        // Fast Float64Array restore using set()
        this.matrix.set(save.matrix);
        const savedBasicIndexes = save.varIndexByRow;
        const height = this.height;
        for (let c = 0; c < height; c += 1) {
            this.varIndexByRow[c] = savedBasicIndexes[c];
        }
        this.varIndexByRow.length = height;
        const savedNonBasicIndexes = save.varIndexByCol;
        const width = this.width;
        for (let r = 0; r < width; r += 1) {
            this.varIndexByCol[r] = savedNonBasicIndexes[r];
        }
        this.varIndexByCol.length = width;
        const savedRows = save.rowByVarIndex;
        const savedCols = save.colByVarIndex;
        for (let v = 0; v < this.nVars; v += 1) {
            this.rowByVarIndex[v] = savedRows[v];
            this.colByVarIndex[v] = savedCols[v];
        }
        if (save.optionalObjectives.length > 0 && this.optionalObjectives.length > 0) {
            this.optionalObjectives = [];
            this.optionalObjectivePerPriority = {};
            for (let o = 0; o < save.optionalObjectives.length; o++) {
                const optionalObjectiveCopy = save.optionalObjectives[o].copy();
                this.optionalObjectives[o] = optionalObjectiveCopy;
                this.optionalObjectivePerPriority[optionalObjectiveCopy.priority] =
                    optionalObjectiveCopy;
                this.objectivesByPriority[optionalObjectiveCopy.priority] = optionalObjectiveCopy;
            }
        }
    }

    // ========== Integer Property Functions ==========
    /**
     * Count how many integer variables currently have integral values.
     */
    function countIntegerValues() {
        let count = 0;
        const width = this.width;
        const matrix = this.matrix;
        const rhsColumn = this.rhsColumn;
        for (let r = 1; r < this.height; r += 1) {
            const variable = this.variablesPerIndex[this.varIndexByRow[r]];
            if (variable !== undefined && variable.isInteger) {
                const value = matrix[r * width + rhsColumn];
                const decimalPart = value - Math.floor(value);
                if (decimalPart < this.precision && -decimalPart < this.precision) {
                    count += 1;
                }
            }
        }
        return count;
    }
    /**
     * Check if all integer variables have integral values.
     * Returns true if the current solution is integral.
     */
    function isIntegral() {
        const width = this.width;
        const matrix = this.matrix;
        const rhsColumn = this.rhsColumn;
        const integerVariables = this.model.integerVariables;
        const nIntegerVars = integerVariables.length;
        // Cache array reference for faster access in hot loop
        const rowByVarIndex = this.rowByVarIndex;
        const precision = this.precision;
        for (let v = 0; v < nIntegerVars; v++) {
            const varIndex = integerVariables[v].index;
            const row = rowByVarIndex[varIndex];
            if (row !== -1) {
                const value = matrix[row * width + rhsColumn];
                if (Math.abs(value - Math.round(value)) > precision) {
                    return false;
                }
            }
        }
        return true;
    }
    /**
     * Compute a measure of how fractional the current solution is.
     * Used for evaluating the quality of cutting planes.
     */
    function computeFractionalVolume(ignoreIntegerValues) {
        let volume = -1;
        const width = this.width;
        const matrix = this.matrix;
        const rhsColumn = this.rhsColumn;
        const height = this.height;
        // Cache array references for faster access in hot loop
        const variablesPerIndex = this.variablesPerIndex;
        const varIndexByRow = this.varIndexByRow;
        const precision = this.precision;
        for (let r = 1; r < height; r += 1) {
            const variable = variablesPerIndex[varIndexByRow[r]];
            if (variable !== undefined && variable.isInteger) {
                const value = matrix[r * width + rhsColumn];
                const distance = Math.abs(value);
                if (Math.min(distance - Math.floor(distance), Math.floor(distance + 1)) < precision) {
                    if (ignoreIntegerValues !== true) {
                        return 0;
                    }
                }
                else if (volume === -1) {
                    volume = distance;
                }
                else {
                    volume *= distance;
                }
            }
        }
        return volume === -1 ? 0 : volume;
    }
    // ========== Branching Variable Selection ==========
    /**
     * Select the integer variable with the most fractional value.
     * Standard branching strategy - picks the variable closest to 0.5 fractionality.
     */
    function getMostFractionalVar() {
        let biggestFraction = 0;
        let selectedVarIndex = null;
        let selectedVarValue = 0;
        const width = this.width;
        const matrix = this.matrix;
        const rhsColumn = this.rhsColumn;
        const integerVars = this.model.integerVariables;
        const nIntegerVars = integerVars.length;
        // Cache array reference for faster access in hot loop
        const rowByVarIndex = this.rowByVarIndex;
        for (let v = 0; v < nIntegerVars; v += 1) {
            const varIndex = integerVars[v].index;
            const row = rowByVarIndex[varIndex];
            if (row !== -1) {
                const varValue = matrix[row * width + rhsColumn];
                const fraction = Math.abs(varValue - Math.round(varValue));
                if (fraction > biggestFraction) {
                    biggestFraction = fraction;
                    selectedVarIndex = varIndex;
                    selectedVarValue = varValue;
                }
            }
        }
        return { index: selectedVarIndex, value: selectedVarValue };
    }
    /**
     * Select the fractional integer variable with the lowest cost coefficient.
     * Alternative branching strategy that considers objective function impact.
     */
    function getFractionalVarWithLowestCost() {
        let highestCost = Infinity;
        let selectedVarIndex = null;
        let selectedVarValue = null;
        const width = this.width;
        const matrix = this.matrix;
        const rhsColumn = this.rhsColumn;
        const integerVars = this.model.integerVariables;
        const nIntegerVars = integerVars.length;
        for (let v = 0; v < nIntegerVars; v += 1) {
            const variable = integerVars[v];
            const varIndex = variable.index;
            const varRow = this.rowByVarIndex[varIndex];
            if (varRow !== -1) {
                const varValue = matrix[varRow * width + rhsColumn];
                if (Math.abs(varValue - Math.round(varValue)) > this.precision &&
                    variable.cost < highestCost) {
                    highestCost = variable.cost;
                    selectedVarIndex = varIndex;
                    selectedVarValue = varValue;
                }
            }
        }
        return { index: selectedVarIndex, value: selectedVarValue };
    }

    function log(message, force) {
        if (!force) {
            return this;
        }
        // eslint-disable-next-line no-console
        console.log("****", message, "****");
        // eslint-disable-next-line no-console
        console.log("Nb Variables", this.width - 1);
        // eslint-disable-next-line no-console
        console.log("Nb Constraints", this.height - 1);
        // console.log("Variable Ids", this.variablesPerIndex);
        // eslint-disable-next-line no-console
        console.log("Basic Indexes", this.varIndexByRow);
        // eslint-disable-next-line no-console
        console.log("Non Basic Indexes", this.varIndexByCol);
        // eslint-disable-next-line no-console
        console.log("Rows", this.rowByVarIndex);
        // eslint-disable-next-line no-console
        console.log("Cols", this.colByVarIndex);
        const digitPrecision = 5;
        const matrix = this.matrix;
        const width = this.width;
        let varNameRowString = "";
        const spacePerColumn = [" "];
        let j;
        let c;
        let varIndex;
        let varName;
        let varNameLength;
        let valueSpace;
        let nameSpace;
        for (c = 1; c < this.width; c += 1) {
            varIndex = this.varIndexByCol[c];
            const variable = this.variablesPerIndex[varIndex];
            if (variable === undefined) {
                varName = "c" + varIndex;
            }
            else {
                varName = variable.id;
            }
            varNameLength = varName.length;
            valueSpace = " ";
            nameSpace = "\t";
            if (varNameLength > 5) {
                valueSpace += " ";
            }
            else {
                nameSpace += "\t";
            }
            spacePerColumn[c] = valueSpace;
            varNameRowString += nameSpace + varName;
        }
        // eslint-disable-next-line no-console
        console.log(varNameRowString);
        let signSpace;
        const costRowOffset = this.costRowIndex * width;
        let firstRowString = "\t";
        for (j = 1; j < this.width; j += 1) {
            signSpace = "\t";
            firstRowString += signSpace;
            firstRowString += spacePerColumn[j];
            firstRowString += matrix[costRowOffset + j].toFixed(digitPrecision);
        }
        signSpace = "\t";
        firstRowString += signSpace + spacePerColumn[0] + matrix[costRowOffset].toFixed(digitPrecision);
        // eslint-disable-next-line no-console
        console.log(firstRowString + "\tZ");
        for (let r = 1; r < this.height; r += 1) {
            const rowOffset = r * width;
            let rowString = "\t";
            for (c = 1; c < this.width; c += 1) {
                signSpace = "\t";
                rowString +=
                    signSpace + spacePerColumn[c] + matrix[rowOffset + c].toFixed(digitPrecision);
            }
            signSpace = "\t";
            rowString += signSpace + spacePerColumn[0] + matrix[rowOffset].toFixed(digitPrecision);
            varIndex = this.varIndexByRow[r];
            const variable = this.variablesPerIndex[varIndex];
            if (variable === undefined) {
                varName = "c" + varIndex;
            }
            else {
                varName = variable.id;
            }
            // eslint-disable-next-line no-console
            console.log(rowString + "\t" + varName);
        }
        // eslint-disable-next-line no-console
        console.log("");
        const nOptionalObjectives = this.optionalObjectives.length;
        if (nOptionalObjectives > 0) {
            // eslint-disable-next-line no-console
            console.log("    Optional objectives:");
            for (let o = 0; o < nOptionalObjectives; o += 1) {
                const reducedCosts = this.optionalObjectives[o].reducedCosts;
                let reducedCostsString = "";
                for (j = 1; j < this.width; j += 1) {
                    signSpace = reducedCosts[j] < 0 ? "" : " ";
                    reducedCostsString += signSpace;
                    reducedCostsString += spacePerColumn[j];
                    reducedCostsString += reducedCosts[j].toFixed(digitPrecision);
                }
                signSpace = reducedCosts[0] < 0 ? "" : " ";
                reducedCostsString +=
                    signSpace + spacePerColumn[0] + reducedCosts[0].toFixed(digitPrecision);
                // eslint-disable-next-line no-console
                console.log(reducedCostsString + " z" + o);
            }
        }
        // eslint-disable-next-line no-console
        console.log("Feasible?", this.feasible);
        // eslint-disable-next-line no-console
        console.log("evaluation", this.evaluation);
        return this;
    }

    /**
     * @file src/tableau/tableau.ts
     * @description Core Tableau class for the simplex algorithm
     *
     * The Tableau represents the LP problem in matrix form and provides:
     * - Matrix storage using Float64Array for numerical precision
     * - Variable and constraint index management
     * - Simplex operations (via bound function imports)
     * - Branch-and-cut integration for MIP solving
     * - Save/restore for backtracking during B&B
     */
    function createOptionalObjective(priority, nColumns, reducedCosts) {
        return {
            priority,
            reducedCosts: reducedCosts ? reducedCosts.slice() : new Array(nColumns).fill(0),
            copy() {
                return createOptionalObjective(this.priority, this.reducedCosts.length, this.reducedCosts);
            },
        };
    }
    class Tableau {
        constructor(precision = 1e-8, branchAndCutService) {
            this.model = null;
            this.matrix = new Float64Array(0);
            this.width = 0;
            this.height = 0;
            this.costRowIndex = 0;
            this.rhsColumn = 0;
            this.variablesPerIndex = [];
            this.unrestrictedVars = {};
            this.feasible = true;
            this.evaluation = 0;
            this.simplexIters = 0;
            this.varIndexByRow = [];
            this.varIndexByCol = [];
            this.rowByVarIndex = [];
            this.colByVarIndex = [];
            this.optionalObjectives = [];
            this.objectivesByPriority = {};
            this.optionalObjectivePerPriority = {};
            this.savedState = null;
            this.availableIndexes = [];
            this.lastElementIndex = 0;
            this.variables = [];
            this.nVars = 0;
            this.bounded = true;
            this.unboundedVarIndex = null;
            this.branchAndCutIterations = 0;
            this.bestPossibleEval = 0;
            // Partial pricing state for phase2 optimization
            this.pricingBatchStart = 1;
            this.pricingBatchSize = 0; // 0 means auto-compute based on problem size
            this.precision = precision;
            this.branchAndCutService = branchAndCutService !== null && branchAndCutService !== void 0 ? branchAndCutService : createBranchAndCutService();
        }
        // ========== Core Simplex Operations ==========
        simplex() {
            simplex.call(this);
            return this;
        }
        phase1() {
            return phase1.call(this);
        }
        phase2() {
            return phase2.call(this);
        }
        /**
         * Dual simplex for warm-starting after adding bound constraints.
         * Use when solution is dual feasible but may be primal infeasible.
         * @returns Number of iterations, or -1 if dual infeasible
         */
        dualSimplex() {
            return dualSimplex.call(this);
        }
        pivot(pivotRowIndex, pivotColumnIndex) {
            pivot.call(this, pivotRowIndex, pivotColumnIndex);
        }
        checkForCycles(varIndexes) {
            return checkForCycles.call(this, varIndexes);
        }
        // ========== Integer/MIP Properties ==========
        countIntegerValues() {
            return countIntegerValues.call(this);
        }
        isIntegral() {
            return isIntegral.call(this);
        }
        computeFractionalVolume(ignoreIntegerValues) {
            return computeFractionalVolume.call(this, ignoreIntegerValues);
        }
        // ========== Cutting Strategies ==========
        addCutConstraints(branchingCuts) {
            addCutConstraints.call(this, branchingCuts);
        }
        applyMIRCuts() {
            applyMIRCuts.call(this);
        }
        addLowerBoundMIRCut(rowIndex) {
            return addLowerBoundMIRCut.call(this, rowIndex);
        }
        addUpperBoundMIRCut(rowIndex) {
            return addUpperBoundMIRCut.call(this, rowIndex);
        }
        // ========== Branching Strategies ==========
        getMostFractionalVar() {
            return getMostFractionalVar.call(this);
        }
        getFractionalVarWithLowestCost() {
            return getFractionalVarWithLowestCost.call(this);
        }
        // ========== Dynamic Modification ==========
        putInBase(varIndex) {
            return putInBase.call(this, varIndex);
        }
        takeOutOfBase(varIndex) {
            return takeOutOfBase.call(this, varIndex);
        }
        updateVariableValues() {
            updateVariableValues.call(this);
        }
        updateRightHandSide(constraint, difference) {
            updateRightHandSide.call(this, constraint, difference);
        }
        updateConstraintCoefficient(constraint, variable, difference) {
            updateConstraintCoefficient.call(this, constraint, variable, difference);
        }
        updateCost(variable, difference) {
            updateCost.call(this, variable, difference);
        }
        addConstraint(constraint) {
            addConstraint.call(this, constraint);
        }
        removeConstraint(constraint) {
            removeConstraint.call(this, constraint);
        }
        addVariable(variable) {
            addVariable.call(this, variable);
        }
        removeVariable(variable) {
            removeVariable.call(this, variable);
        }
        // ========== Backup/Restore ==========
        copy() {
            return copy.call(this);
        }
        save() {
            save.call(this);
        }
        restore() {
            restore.call(this);
        }
        // ========== Debug ==========
        log(message) {
            log.call(this, message);
            return this;
        }
        // ========== Branch and Cut ==========
        applyCuts(branchingCuts) {
            this.branchAndCutService.applyCuts(this, branchingCuts);
        }
        branchAndCut() {
            this.branchAndCutService.branchAndCut(this);
        }
        // ========== Solution ==========
        solve() {
            var _a, _b;
            if (((_b = (_a = this.model) === null || _a === void 0 ? void 0 : _a.getNumberOfIntegerVariables()) !== null && _b !== void 0 ? _b : 0) > 0) {
                this.branchAndCut();
            }
            else {
                this.simplex();
            }
            this.updateVariableValues();
            return this.getSolution();
        }
        getSolution() {
            var _a, _b, _c;
            const evaluation = ((_a = this.model) === null || _a === void 0 ? void 0 : _a.isMinimization) === true ? this.evaluation : -this.evaluation;
            if (((_c = (_b = this.model) === null || _b === void 0 ? void 0 : _b.getNumberOfIntegerVariables()) !== null && _c !== void 0 ? _c : 0) > 0) {
                return new MilpSolution(this, evaluation, this.feasible, this.bounded, this.branchAndCutIterations);
            }
            else {
                return new Solution(this, evaluation, this.feasible, this.bounded);
            }
        }
        // ========== Initialization ==========
        setOptionalObjective(priority, column, cost) {
            let objectiveForPriority = this.objectivesByPriority[priority];
            if (objectiveForPriority === undefined) {
                const nColumns = Math.max(this.width, column + 1);
                objectiveForPriority = createOptionalObjective(priority, nColumns);
                this.objectivesByPriority[priority] = objectiveForPriority;
                this.optionalObjectivePerPriority[priority] = objectiveForPriority;
                this.optionalObjectives.push(objectiveForPriority);
                this.optionalObjectives.sort((a, b) => a.priority - b.priority);
            }
            objectiveForPriority.reducedCosts[column] = cost;
        }
        initialize(width, height, variables, unrestrictedVars) {
            this.variables = variables;
            this.unrestrictedVars = unrestrictedVars;
            this.width = width;
            this.height = height;
            this.matrix = new Float64Array(width * height);
            this.varIndexByRow = new Array(this.height);
            this.varIndexByCol = new Array(this.width);
            this.varIndexByRow[0] = -1;
            this.varIndexByCol[0] = -1;
            this.nVars = width + height - 2;
            this.rowByVarIndex = new Array(this.nVars);
            this.colByVarIndex = new Array(this.nVars);
            this.lastElementIndex = this.nVars;
        }
        _resetMatrix() {
            if (this.model === null) {
                throw new Error("[Tableau._resetMatrix] Model not set");
            }
            const matrix = this.matrix;
            const width = this.width;
            const variables = this.model.variables;
            const constraints = this.model.constraints;
            const nVars = variables.length;
            const nConstraints = constraints.length;
            const coeff = this.model.isMinimization === true ? -1 : 1;
            for (let v = 0; v < nVars; v += 1) {
                const variable = variables[v];
                const priority = variable.priority;
                const cost = coeff * variable.cost;
                if (priority === 0) {
                    matrix[v + 1] = cost;
                }
                else {
                    this.setOptionalObjective(priority, v + 1, cost);
                }
                const varIndex = variables[v].index;
                this.rowByVarIndex[varIndex] = -1;
                this.colByVarIndex[varIndex] = v + 1;
                this.varIndexByCol[v + 1] = varIndex;
            }
            let rowIndex = 1;
            for (let c = 0; c < nConstraints; c += 1) {
                const constraint = constraints[c];
                const constraintIndex = constraint.index;
                this.rowByVarIndex[constraintIndex] = rowIndex;
                this.colByVarIndex[constraintIndex] = -1;
                this.varIndexByRow[rowIndex] = constraintIndex;
                const terms = constraint.terms;
                const nTerms = terms.length;
                const rowOffset = rowIndex * width;
                rowIndex++;
                if (constraint.isUpperBound) {
                    for (let t = 0; t < nTerms; t += 1) {
                        const term = terms[t];
                        const column = this.colByVarIndex[term.variable.index];
                        matrix[rowOffset + column] = term.coefficient;
                    }
                    matrix[rowOffset] = constraint.rhs;
                }
                else {
                    for (let t = 0; t < nTerms; t += 1) {
                        const term = terms[t];
                        const column = this.colByVarIndex[term.variable.index];
                        matrix[rowOffset + column] = -term.coefficient;
                    }
                    matrix[rowOffset] = -constraint.rhs;
                }
            }
        }
        setModel(model) {
            this.model = model;
            const width = model.nVariables + 1;
            const height = model.nConstraints + 1;
            this.initialize(width, height, model.variables, model.unrestrictedVariables);
            this._resetMatrix();
            return this;
        }
        getNewElementIndex() {
            if (this.availableIndexes.length > 0) {
                return this.availableIndexes.pop();
            }
            const index = this.lastElementIndex;
            this.lastElementIndex += 1;
            return index;
        }
        density() {
            let density = 0;
            const matrix = this.matrix;
            const width = this.width;
            for (let r = 0; r < this.height; r++) {
                const rowOffset = r * width;
                for (let c = 0; c < width; c++) {
                    if (matrix[rowOffset + c] !== 0) {
                        density += 1;
                    }
                }
            }
            return density / (this.height * this.width);
        }
        setEvaluation() {
            const roundingCoeff = Math.round(1 / this.precision);
            const evaluation = this.matrix[this.costRowIndex * this.width + this.rhsColumn];
            const roundedEvaluation = Math.round((Number.EPSILON + evaluation) * roundingCoeff) / roundingCoeff;
            this.evaluation = roundedEvaluation;
            if (this.simplexIters === 0) {
                this.bestPossibleEval = roundedEvaluation;
            }
        }
    }

    /**
     * Probing: Temporarily fix binary variables and propagate to find implications.
     * If fixing x=0 causes infeasibility, then x must be 1 (and vice versa).
     * If both x=0 and x=1 imply y has same bound, that bound is valid.
     */
    /**
     * Coefficient tightening for knapsack-like constraints.
     * If a coefficient is larger than the remaining capacity, reduce it.
     */
    function tightenCoefficients(model, result) {
        var _a, _b, _c, _d, _e, _f, _g;
        let changed = false;
        for (const constraint of model.constraints) {
            if (result.removedConstraints.has(constraint))
                continue;
            if (!constraint.isUpperBound)
                continue; // Only for <= constraints
            // Calculate min activity (all variables at their lower bounds)
            let minActivity = 0;
            for (const term of constraint.terms) {
                if (result.fixedVariables.has(term.variable)) {
                    minActivity += term.coefficient * result.fixedVariables.get(term.variable);
                }
                else {
                    const bounds = (_a = result.tightenedBounds.get(term.variable)) !== null && _a !== void 0 ? _a : {};
                    const lower = (_b = bounds.lower) !== null && _b !== void 0 ? _b : 0;
                    if (term.coefficient > 0) {
                        minActivity += term.coefficient * lower;
                    }
                    else {
                        const upper = (_c = bounds.upper) !== null && _c !== void 0 ? _c : Infinity;
                        minActivity += term.coefficient * upper;
                    }
                }
            }
            // For each variable, check if coefficient can be tightened
            const slack = constraint.rhs - minActivity;
            if (slack < 0)
                continue; // Constraint may be infeasible
            for (const term of constraint.terms) {
                if (result.fixedVariables.has(term.variable))
                    continue;
                if (!term.variable.isInteger)
                    continue;
                if (term.coefficient <= 0)
                    continue;
                const bounds = (_d = result.tightenedBounds.get(term.variable)) !== null && _d !== void 0 ? _d : {};
                const lower = (_e = bounds.lower) !== null && _e !== void 0 ? _e : 0;
                const upper = (_f = bounds.upper) !== null && _f !== void 0 ? _f : 1;
                // For binary variables: if coeff > slack, can reduce to slack
                if (lower >= -0.5 && upper <= 1.5) {
                    const effectiveCoeff = term.coefficient * (upper - lower);
                    if (effectiveCoeff > slack + 1e-6) {
                        // Could tighten coefficient - but we don't modify the model
                        // Instead, derive an upper bound on the variable
                        const impliedUpper = lower + slack / term.coefficient;
                        if (impliedUpper < upper - 1e-6) {
                            const current = (_g = result.tightenedBounds.get(term.variable)) !== null && _g !== void 0 ? _g : {};
                            if (!current.upper || impliedUpper < current.upper) {
                                result.tightenedBounds.set(term.variable, {
                                    ...current,
                                    upper: impliedUpper,
                                });
                                result.stats.boundsTightened++;
                                changed = true;
                            }
                        }
                    }
                }
            }
        }
        return changed;
    }
    /**
     * Detect redundant constraints using activity bounds.
     * If max activity <= RHS for <= constraint, it's redundant.
     */
    function removeRedundantConstraints(model, result) {
        var _a, _b, _c;
        let changed = false;
        for (const constraint of model.constraints) {
            if (result.removedConstraints.has(constraint))
                continue;
            // Calculate activity bounds
            let minActivity = 0;
            let maxActivity = 0;
            for (const term of constraint.terms) {
                const fixed = result.fixedVariables.get(term.variable);
                if (fixed !== undefined) {
                    minActivity += term.coefficient * fixed;
                    maxActivity += term.coefficient * fixed;
                    continue;
                }
                const bounds = (_a = result.tightenedBounds.get(term.variable)) !== null && _a !== void 0 ? _a : {};
                const lower = (_b = bounds.lower) !== null && _b !== void 0 ? _b : 0;
                const upper = (_c = bounds.upper) !== null && _c !== void 0 ? _c : Infinity;
                if (term.coefficient > 0) {
                    minActivity += term.coefficient * lower;
                    maxActivity += term.coefficient * (upper === Infinity ? 1e10 : upper);
                }
                else {
                    minActivity += term.coefficient * (upper === Infinity ? 1e10 : upper);
                    maxActivity += term.coefficient * lower;
                }
            }
            // Check redundancy
            if (constraint.isUpperBound) {
                // <= constraint is redundant if max activity <= RHS
                if (maxActivity <= constraint.rhs + 1e-6) {
                    result.removedConstraints.add(constraint);
                    result.stats.constraintsRemoved++;
                    changed = true;
                }
                // Infeasible if min activity > RHS
                if (minActivity > constraint.rhs + 1e-6) {
                    result.isInfeasible = true;
                    return false;
                }
            }
            else {
                // >= constraint is redundant if min activity >= RHS
                if (minActivity >= constraint.rhs - 1e-6) {
                    result.removedConstraints.add(constraint);
                    result.stats.constraintsRemoved++;
                    changed = true;
                }
                // Infeasible if max activity < RHS
                if (maxActivity < constraint.rhs - 1e-6) {
                    result.isInfeasible = true;
                    return false;
                }
            }
        }
        return changed;
    }
    /**
     * Presolve reductions for Mixed Integer Programs.
     * Based on techniques from COIN-OR CBC, CPLEX, and Gurobi.
     *
     * Key techniques:
     * 1. Fixed variable removal
     * 2. Singleton row detection
     * 3. Bound tightening
     * 4. Redundant constraint removal
     * 5. Probing (for binary variables)
     * 6. Coefficient tightening
     */
    function presolve(model) {
        var _a, _b;
        const result = {
            fixedVariables: new Map(),
            removedConstraints: new Set(),
            tightenedBounds: new Map(),
            isInfeasible: false,
            stats: {
                variablesFixed: 0,
                constraintsRemoved: 0,
                boundsTightened: 0,
            },
        };
        // Multiple passes for propagation
        let changed = true;
        let passes = 0;
        const maxPasses = 5;
        while (changed && passes < maxPasses) {
            changed = false;
            passes++;
            // Pass 1: Singleton rows - constraints with single variable
            for (const constraint of model.constraints) {
                if (result.removedConstraints.has(constraint))
                    continue;
                const activeTerms = constraint.terms.filter((t) => !result.fixedVariables.has(t.variable));
                if (activeTerms.length === 0) {
                    // All variables fixed - check feasibility
                    let lhs = 0;
                    for (const term of constraint.terms) {
                        const fixedVal = result.fixedVariables.get(term.variable);
                        if (fixedVal !== undefined) {
                            lhs += term.coefficient * fixedVal;
                        }
                    }
                    const satisfied = constraint.isUpperBound
                        ? lhs <= constraint.rhs + 1e-6
                        : lhs >= constraint.rhs - 1e-6;
                    if (!satisfied) {
                        result.isInfeasible = true;
                        return result;
                    }
                    result.removedConstraints.add(constraint);
                    result.stats.constraintsRemoved++;
                    changed = true;
                }
                else if (activeTerms.length === 1) {
                    // Singleton row - can fix or tighten bounds
                    const term = activeTerms[0];
                    const variable = term.variable;
                    const coeff = term.coefficient;
                    // Calculate RHS adjustment for fixed variables
                    let rhsAdj = constraint.rhs;
                    for (const t of constraint.terms) {
                        if (t.variable !== variable) {
                            const fixedVal = result.fixedVariables.get(t.variable);
                            if (fixedVal !== undefined) {
                                rhsAdj -= t.coefficient * fixedVal;
                            }
                        }
                    }
                    const bound = rhsAdj / coeff;
                    if (constraint.isUpperBound) {
                        // x <= bound (if coeff > 0) or x >= bound (if coeff < 0)
                        if (coeff > 0) {
                            // Upper bound
                            const current = result.tightenedBounds.get(variable);
                            if (!(current === null || current === void 0 ? void 0 : current.upper) || bound < current.upper) {
                                result.tightenedBounds.set(variable, {
                                    ...current,
                                    upper: bound,
                                });
                                result.stats.boundsTightened++;
                                changed = true;
                            }
                        }
                        else {
                            // Lower bound (coefficient is negative)
                            const current = result.tightenedBounds.get(variable);
                            if (!(current === null || current === void 0 ? void 0 : current.lower) || bound > current.lower) {
                                result.tightenedBounds.set(variable, {
                                    ...current,
                                    lower: bound,
                                });
                                result.stats.boundsTightened++;
                                changed = true;
                            }
                        }
                    }
                    result.removedConstraints.add(constraint);
                    result.stats.constraintsRemoved++;
                }
            }
            // Pass 2: Check for fixed variables from bounds
            for (const [variable, bounds] of result.tightenedBounds) {
                if (result.fixedVariables.has(variable))
                    continue;
                if (bounds.lower !== undefined && bounds.upper !== undefined) {
                    if (bounds.lower > bounds.upper + 1e-6) {
                        result.isInfeasible = true;
                        return result;
                    }
                    if (Math.abs(bounds.lower - bounds.upper) < 1e-6) {
                        // Variable is fixed
                        let fixedValue = bounds.lower;
                        // If integer, round to nearest integer
                        if (variable.isInteger) {
                            fixedValue = Math.round(fixedValue);
                        }
                        result.fixedVariables.set(variable, fixedValue);
                        result.stats.variablesFixed++;
                        changed = true;
                    }
                }
                // Binary variables with lower bound >= 0.5 are fixed to 1
                if (variable.isInteger && bounds.lower !== undefined && bounds.lower >= 0.5) {
                    const upperBound = (_a = bounds.upper) !== null && _a !== void 0 ? _a : Infinity;
                    if (upperBound <= 1.5) {
                        result.fixedVariables.set(variable, 1);
                        result.stats.variablesFixed++;
                        changed = true;
                    }
                }
                // Binary variables with upper bound <= 0.5 are fixed to 0
                if (variable.isInteger && bounds.upper !== undefined && bounds.upper <= 0.5) {
                    const lowerBound = (_b = bounds.lower) !== null && _b !== void 0 ? _b : 0;
                    if (lowerBound >= -0.5) {
                        result.fixedVariables.set(variable, 0);
                        result.stats.variablesFixed++;
                        changed = true;
                    }
                }
            }
            // Pass 3: Remove redundant constraints using activity bounds
            if (removeRedundantConstraints(model, result)) {
                changed = true;
            }
            if (result.isInfeasible)
                return result;
            // Pass 4: Coefficient tightening for knapsack-like constraints
            if (tightenCoefficients(model, result)) {
                changed = true;
            }
            // Pass 5: Probing for binary variables (disabled - needs more work on equality constraints)
            // TODO: Fix probing to handle equality constraints correctly
            // if (passes <= 2 && model.integerVariables.length > 0) {
            //     const probeLimit = Math.min(50, model.integerVariables.length);
            //     if (probe(model, result, probeLimit)) {
            //         changed = true;
            //     }
            //     if (result.isInfeasible) return result;
            // }
        }
        return result;
    }

    /**
     * @file src/model.ts
     * @description Model class for LP/MIP problem representation
     *
     * Provides the programmatic API for building optimization problems:
     * - Add variables with costs and integer constraints
     * - Define constraints (<=, >=, =) with coefficients
     * - Load problems from JSON model definitions
     * - Dynamic model modification after initialization
     *
     * The Model converts high-level problem definitions into the internal
     * Tableau representation used by the simplex algorithm.
     */
    class Model {
        constructor(precision, name, branchAndCutService) {
            this.tableau = new Tableau(precision, branchAndCutService);
            this.name = name;
            this.variables = [];
            this.integerVariables = [];
            this.unrestrictedVariables = {};
            this.constraints = [];
            this.nConstraints = 0;
            this.nVariables = 0;
            this.isMinimization = true;
            this.tableauInitialized = false;
            this.relaxationIndex = 1;
            this.useMIRCuts = false;
            this.checkForCycles = true;
            // Collect diagnostic messages for debugging without console output
            this.messages = [];
            this.availableIndexes = [];
            this.lastElementIndex = 0;
            this.usePresolve = true;
            this.presolveResult = null;
        }
        minimize() {
            this.isMinimization = true;
            return this;
        }
        maximize() {
            this.isMinimization = false;
            return this;
        }
        _getNewElementIndex() {
            if (this.availableIndexes.length > 0) {
                return this.availableIndexes.pop();
            }
            const index = this.lastElementIndex;
            this.lastElementIndex += 1;
            return index;
        }
        _addConstraint(constraint) {
            const slackVariable = constraint.slack;
            this.tableau.variablesPerIndex[slackVariable.index] = slackVariable;
            this.constraints.push(constraint);
            this.nConstraints += 1;
            if (this.tableauInitialized === true) {
                this.tableau.addConstraint(constraint);
            }
        }
        smallerThan(rhs) {
            const constraint = new Constraint(rhs, true, this.tableau.getNewElementIndex(), this);
            this._addConstraint(constraint);
            return constraint;
        }
        greaterThan(rhs) {
            const constraint = new Constraint(rhs, false, this.tableau.getNewElementIndex(), this);
            this._addConstraint(constraint);
            return constraint;
        }
        equal(rhs) {
            const constraintUpper = new Constraint(rhs, true, this.tableau.getNewElementIndex(), this);
            this._addConstraint(constraintUpper);
            const constraintLower = new Constraint(rhs, false, this.tableau.getNewElementIndex(), this);
            this._addConstraint(constraintLower);
            return new Equality(constraintUpper, constraintLower);
        }
        addVariable(cost, id, isInteger, isUnrestricted, priority) {
            if (typeof priority === "string") {
                switch (priority) {
                    case "required":
                        priority = 0;
                        break;
                    case "strong":
                        priority = 1;
                        break;
                    case "medium":
                        priority = 2;
                        break;
                    case "weak":
                        priority = 3;
                        break;
                    default:
                        priority = 0;
                        break;
                }
            }
            const varIndex = this.tableau.getNewElementIndex();
            const identifier = id !== null && id !== void 0 ? id : "v" + varIndex;
            const normalizedCost = cost !== null && cost !== void 0 ? cost : 0;
            const normalizedPriority = priority !== null && priority !== void 0 ? priority : 0;
            let variable;
            if (isInteger) {
                const integerVariable = new IntegerVariable(identifier, normalizedCost, varIndex, normalizedPriority);
                this.integerVariables.push(integerVariable);
                variable = integerVariable;
            }
            else {
                variable = new Variable(identifier, normalizedCost, varIndex, normalizedPriority);
            }
            this.variables.push(variable);
            this.tableau.variablesPerIndex[varIndex] = variable;
            if (isUnrestricted) {
                this.unrestrictedVariables[varIndex] = true;
            }
            this.nVariables += 1;
            if (this.tableauInitialized === true) {
                this.tableau.addVariable(variable);
            }
            return variable;
        }
        _removeConstraint(constraint) {
            const idx = this.constraints.indexOf(constraint);
            if (idx === -1) {
                // eslint-disable-next-line no-console
                console.warn("[Model.removeConstraint] Constraint not present in model");
                return;
            }
            this.constraints.splice(idx, 1);
            this.nConstraints -= 1;
            if (this.tableauInitialized === true) {
                this.tableau.removeConstraint(constraint);
            }
            if (constraint.relaxation) {
                this.removeVariable(constraint.relaxation);
            }
        }
        //-------------------------------------------------------------------
        // For dynamic model modification
        //-------------------------------------------------------------------
        removeConstraint(constraint) {
            if (constraint.isEquality) {
                const equalityConstraint = constraint;
                this._removeConstraint(equalityConstraint.upperBound);
                this._removeConstraint(equalityConstraint.lowerBound);
            }
            else {
                this._removeConstraint(constraint);
            }
            return this;
        }
        removeVariable(variable) {
            const idx = this.variables.indexOf(variable);
            if (idx === -1) {
                // eslint-disable-next-line no-console
                console.warn("[Model.removeVariable] Variable not present in model");
                return;
            }
            this.variables.splice(idx, 1);
            if (this.tableauInitialized === true) {
                this.tableau.removeVariable(variable);
            }
            return this;
        }
        updateRightHandSide(constraint, difference) {
            if (this.tableauInitialized === true) {
                this.tableau.updateRightHandSide(constraint, difference);
            }
            return this;
        }
        updateConstraintCoefficient(constraint, variable, difference) {
            if (this.tableauInitialized === true) {
                this.tableau.updateConstraintCoefficient(constraint, variable, difference);
            }
            return this;
        }
        setCost(cost, variable) {
            let difference = cost - variable.cost;
            if (this.isMinimization === false) {
                difference = -difference;
            }
            variable.cost = cost;
            this.tableau.updateCost(variable, difference);
            return this;
        }
        //-------------------------------------------------------------------
        //-------------------------------------------------------------------
        loadJson(jsonModel) {
            this.isMinimization = jsonModel.opType !== "max";
            const variables = jsonModel.variables;
            const constraints = jsonModel.constraints;
            const constraintsMin = {};
            const constraintsMax = {};
            // Instantiating constraints
            const constraintIds = Object.keys(constraints);
            const nConstraintIds = constraintIds.length;
            for (let c = 0; c < nConstraintIds; c += 1) {
                const constraintId = constraintIds[c];
                const constraint = constraints[constraintId];
                const equal = constraint.equal;
                const weight = constraint.weight;
                const priority = constraint.priority;
                const relaxed = weight !== undefined || priority !== undefined;
                let lowerBound;
                let upperBound;
                if (equal === undefined) {
                    const min = constraint.min;
                    if (min !== undefined) {
                        lowerBound = this.greaterThan(min);
                        constraintsMin[constraintId] = lowerBound;
                        if (relaxed) {
                            lowerBound.relax(weight, priority);
                        }
                    }
                    const max = constraint.max;
                    if (max !== undefined) {
                        upperBound = this.smallerThan(max);
                        constraintsMax[constraintId] = upperBound;
                        if (relaxed) {
                            upperBound.relax(weight, priority);
                        }
                    }
                }
                else {
                    lowerBound = this.greaterThan(equal);
                    constraintsMin[constraintId] = lowerBound;
                    upperBound = this.smallerThan(equal);
                    constraintsMax[constraintId] = upperBound;
                    const equality = new Equality(lowerBound, upperBound);
                    if (relaxed) {
                        equality.relax(weight, priority);
                    }
                }
            }
            const variableIds = Object.keys(variables);
            const nVariables = variableIds.length;
            // Parse solver options
            this.tolerance = jsonModel.tolerance || 0;
            if (jsonModel.timeout) {
                this.timeout = jsonModel.timeout;
            }
            // Options object takes precedence over top-level properties
            if (jsonModel.options) {
                if (jsonModel.options.timeout) {
                    this.timeout = jsonModel.options.timeout;
                }
                if (this.tolerance === 0) {
                    this.tolerance = jsonModel.options.tolerance || 0;
                }
                if (jsonModel.options.useMIRCuts) {
                    this.useMIRCuts = jsonModel.options.useMIRCuts;
                }
                // Cycle detection defaults to true
                if (typeof jsonModel.options.exitOnCycles === "undefined") {
                    this.checkForCycles = true;
                }
                else {
                    this.checkForCycles = jsonModel.options.exitOnCycles;
                }
                if (jsonModel.options.keep_solutions) {
                    this.keep_solutions = jsonModel.options.keep_solutions;
                }
                else {
                    this.keep_solutions = false;
                }
                if (jsonModel.options.presolve !== undefined) {
                    this.usePresolve = jsonModel.options.presolve;
                }
            }
            const integerVarIds = jsonModel.ints || {};
            const binaryVarIds = jsonModel.binaries || {};
            const unrestrictedVarIds = jsonModel.unrestricted || {};
            // Instantiating variables and constraint terms
            const objectiveName = jsonModel.optimize;
            // Check if objectiveName exists as a coefficient key in any variable.
            // If not, and it matches a variable name, the user wants to optimize
            // that variable directly (implicit cost of 1).
            const objectiveIsAttribute = variableIds.some((id) => objectiveName in variables[id]);
            const objectiveIsVariable = !objectiveIsAttribute && variableIds.includes(objectiveName);
            for (let v = 0; v < nVariables; v += 1) {
                // Creation of the variables
                const variableId = variableIds[v];
                const variableConstraints = variables[variableId];
                const cost = objectiveIsVariable
                    ? variableId === objectiveName
                        ? 1
                        : 0
                    : variableConstraints[objectiveName] || 0;
                const isBinary = !!binaryVarIds[variableId];
                const isInteger = !!integerVarIds[variableId] || isBinary;
                const isUnrestricted = !!unrestrictedVarIds[variableId];
                const variable = this.addVariable(cost, variableId, isInteger, isUnrestricted);
                if (isBinary) {
                    // Creating an upperbound constraint for this variable
                    this.smallerThan(1).addTerm(1, variable);
                }
                const constraintNames = Object.keys(variableConstraints);
                for (let c = 0; c < constraintNames.length; c += 1) {
                    const constraintName = constraintNames[c];
                    if (constraintName === objectiveName) {
                        continue;
                    }
                    const coefficient = variableConstraints[constraintName];
                    const constraintMin = constraintsMin[constraintName];
                    if (constraintMin !== undefined) {
                        constraintMin.addTerm(coefficient, variable);
                    }
                    const constraintMax = constraintsMax[constraintName];
                    if (constraintMax !== undefined) {
                        constraintMax.addTerm(coefficient, variable);
                    }
                }
            }
            return this;
        }
        //-------------------------------------------------------------------
        //-------------------------------------------------------------------
        getNumberOfIntegerVariables() {
            return this.integerVariables.length;
        }
        solve() {
            // Apply presolve to reduce problem size
            if (this.usePresolve && this.presolveResult === null) {
                this.presolveResult = presolve(this);
                if (this.presolveResult.isInfeasible) {
                    // Problem is infeasible - return early
                    this.tableau.feasible = false;
                    return this.tableau.getSolution();
                }
                // Apply fixed variables to constraints
                this.applyPresolveReductions(this.presolveResult);
            }
            // Setting tableau if not done
            if (this.tableauInitialized === false) {
                this.tableau.setModel(this);
                this.tableauInitialized = true;
            }
            return this.tableau.solve();
        }
        /**
         * Apply presolve reductions to the model.
         * Sets fixed variable values and removes redundant constraints.
         */
        applyPresolveReductions(result) {
            // Set fixed variable values
            for (const [variable, value] of result.fixedVariables) {
                variable.value = value;
                // Zero out the cost since variable is fixed
                variable.cost = 0;
            }
            // Note: We don't actually remove constraints/variables from the model
            // as that would require complex bookkeeping. Instead, the presolve
            // information is used to detect early infeasibility and fix variable values.
            // A more aggressive presolve would rebuild the model without fixed variables.
        }
        isFeasible() {
            return this.tableau.feasible;
        }
        save() {
            this.tableau.save();
        }
        restore() {
            this.tableau.restore();
        }
        activateMIRCuts(useMIRCuts) {
            this.useMIRCuts = useMIRCuts;
        }
        debug(debugCheckForCycles) {
            this.checkForCycles = debugCheckForCycles;
        }
        log(message) {
            return this.tableau.log(message);
        }
    }

    /**
     * Common typos and their corrections for model properties.
     */
    const PROPERTY_TYPOS = {
        optype: "opType",
        OpType: "opType",
        op_type: "opType",
        type: "opType",
        optimise: "optimize",
        Optimize: "optimize",
        objective: "optimize",
        constraint: "constraints",
        Constraints: "constraints",
        variable: "variables",
        Variables: "variables",
        vars: "variables",
        int: "ints",
        integers: "ints",
        Ints: "ints",
        binary: "binaries",
        Binaries: "binaries",
    };
    /**
     * Common typos for constraint properties.
     */
    const CONSTRAINT_TYPOS = {
        minimum: "min",
        maximum: "max",
        Min: "min",
        Max: "max",
        eq: "equal",
        equals: "equal",
        Equal: "equal",
    };
    /**
     * Checks for common typos in model properties and logs warnings.
     *
     * This helps users identify issues like using 'optype' instead of 'opType'
     * which would cause the solver to silently use default behavior.
     */
    function WarnOnTypos(model) {
        const modelKeys = Object.keys(model);
        // Check top-level property typos
        for (const key of modelKeys) {
            const correction = PROPERTY_TYPOS[key];
            if (correction) {
                console.warn(`[jsLPSolver] Warning: Model has '${key}' but expected '${correction}'. ` +
                    `This may cause unexpected behavior.`);
            }
        }
        // Check for missing required properties
        if (!model.optimize && !modelKeys.some((k) => PROPERTY_TYPOS[k] === "optimize")) {
            console.warn(`[jsLPSolver] Warning: Model is missing 'optimize' property. ` +
                `The solver needs to know which attribute to optimize.`);
        }
        if (!model.opType && !modelKeys.some((k) => PROPERTY_TYPOS[k] === "opType")) {
            console.warn(`[jsLPSolver] Warning: Model is missing 'opType' property. ` +
                `Defaulting to 'max'. Use 'opType: "max"' or 'opType: "min"' to be explicit.`);
        }
        // Check constraint property typos
        if (model.constraints) {
            for (const [constraintName, constraint] of Object.entries(model.constraints)) {
                if (typeof constraint === "object" && constraint !== null) {
                    for (const prop of Object.keys(constraint)) {
                        const correction = CONSTRAINT_TYPOS[prop];
                        if (correction) {
                            console.warn(`[jsLPSolver] Warning: Constraint '${constraintName}' has '${prop}' ` +
                                `but expected '${correction}'.`);
                        }
                    }
                }
            }
        }
        return model;
    }
    /**
     * Renames objective attributes that conflict with constraint names.
     *
     * If the optimize attribute is also used as a constraint name, this function
     * creates a new random attribute name to avoid the collision.
     */
    function CleanObjectiveAttributes(model) {
        let fakeAttr;
        let x;
        let z;
        if (typeof model.optimize === "string") {
            if (model.constraints[model.optimize]) {
                // Conflict: objective name matches a constraint name
                fakeAttr = Math.random();
                for (x in model.variables) {
                    if (model.variables[x][model.optimize]) {
                        model.variables[x][fakeAttr] = model.variables[x][model.optimize];
                    }
                }
                model.constraints[fakeAttr] = model.constraints[model.optimize];
                delete model.constraints[model.optimize];
                return model;
            }
            return model;
        }
        else {
            // Multi-objective case
            for (z in model.optimize) {
                if (model.constraints[z]) {
                    if (model.constraints[z] === "equal") {
                        // Can't optimize an equality-constrained attribute
                        delete model.optimize[z];
                    }
                    else {
                        fakeAttr = Math.random();
                        for (x in model.variables) {
                            if (model.variables[x][z]) {
                                model.variables[x][fakeAttr] = model.variables[x][z];
                            }
                        }
                        model.constraints[fakeAttr] = model.constraints[z];
                        delete model.constraints[z];
                    }
                }
            }
            return model;
        }
    }

    var validation = /*#__PURE__*/Object.freeze({
        __proto__: null,
        CleanObjectiveAttributes: CleanObjectiveAttributes,
        WarnOnTypos: WarnOnTypos
    });

    const External = {};

    /**
     * Create a structured, JSON-friendly clone of the incoming model so we can
     * mutate it freely during the multi-objective solve without affecting callers.
     */
    function cloneModel(model) {
        return JSON.parse(JSON.stringify(model));
    }
    function asPolyoptSolution(value) {
        if (value && typeof value === "object") {
            return value;
        }
        throw new Error("Polyopt requires the solver to return an object result.");
    }
    /**
     * Populate the solution object with synthetic values for any objective
     * attribute that is not a standalone variable by aggregating contributions
     * from the model's variables.
     */
    function backfillObjectiveAttributes(solution, workingModel, objectiveKeys) {
        for (const attribute of objectiveKeys) {
            // Skip attributes that already exist as explicit variables.
            if (workingModel.variables[attribute]) {
                continue;
            }
            if (typeof solution[attribute] !== "number") {
                solution[attribute] = 0;
            }
            for (const [variableName, coefficients] of Object.entries(workingModel.variables)) {
                const variableContribution = coefficients[attribute];
                const solvedValue = solution[variableName];
                if (typeof variableContribution === "number" && typeof solvedValue === "number") {
                    solution[attribute] += solvedValue * variableContribution;
                }
            }
        }
    }
    /**
     * Build a string key for a solution vector so we can detect identical vertices
     * (within a small rounding tolerance) and avoid double-counting them when
     * computing the midpoint.
     */
    function buildVectorKey(solution, objectiveKeys) {
        const suffix = objectiveKeys
            .map((key) => {
            const value = solution[key];
            // Round to three decimals so tiny floating point differences do not
            // create distinct vector identifiers.
            return typeof value === "number" ? Math.round(value * 1000) / 1000 : 0;
        })
            .join("-");
        return `base-${suffix}`;
    }
    /**
     * Ensure each vertex object contains all attribute keys and capture the min/max
     * range for each objective across the Pareto set.
     */
    function computeRanges(vertices) {
        var _a;
        const ranges = {};
        // First pass: establish keys and initial ranges from observed values.
        for (const vertex of vertices) {
            for (const [key, value] of Object.entries(vertex)) {
                if (typeof value !== "number") {
                    continue;
                }
                const current = (_a = ranges[key]) !== null && _a !== void 0 ? _a : {
                    min: Number.POSITIVE_INFINITY,
                    max: Number.NEGATIVE_INFINITY,
                };
                ranges[key] = {
                    min: Math.min(current.min, value),
                    max: Math.max(current.max, value),
                };
            }
        }
        // Second pass: fill missing attributes with zero so all vertices are aligned
        // and ranges account for implicit zeros.
        for (const vertex of vertices) {
            for (const key of Object.keys(ranges)) {
                if (typeof vertex[key] !== "number") {
                    vertex[key] = 0;
                }
                ranges[key].min = Math.min(ranges[key].min, vertex[key]);
                ranges[key].max = Math.max(ranges[key].max, vertex[key]);
            }
        }
        // Normalize any untouched ranges to zero so callers never see infinities.
        for (const [key, range] of Object.entries(ranges)) {
            if (!Number.isFinite(range.min)) {
                ranges[key] = { min: 0, max: 0 };
            }
        }
        return ranges;
    }
    /**
     * Solve a model with multiple objective functions by optimizing each objective
     * independently, collecting the resulting Pareto vertices, and solving a
     * derived model that targets the midpoint across all objectives.
     */
    function Polyopt(solver, model) {
        const workingModel = cloneModel(model);
        const objectives = workingModel.optimize;
        const objectiveKeys = Object.keys(objectives);
        if (objectiveKeys.length === 0) {
            throw new Error("Multi-objective solve requires at least one objective definition.");
        }
        // We'll replace optimize/opType repeatedly, so start with a clean slate.
        const workingRecord = workingModel;
        delete workingRecord.optimize;
        delete workingRecord.opType;
        const aggregatedTargets = {};
        const uniqueVectors = new Set();
        const paretoVertices = [];
        for (const key of objectiveKeys) {
            aggregatedTargets[key] = 0;
        }
        for (const objectiveName of objectiveKeys) {
            // Configure the working model to focus solely on the current objective.
            workingModel.optimize = objectiveName;
            workingModel.opType = objectives[objectiveName];
            const solution = asPolyoptSolution(solver.Solve(workingModel, undefined, undefined, true));
            // Ensure attributes that are not explicit variables still get values we can compare.
            backfillObjectiveAttributes(solution, workingModel, objectiveKeys);
            const vectorKey = buildVectorKey(solution, objectiveKeys);
            if (uniqueVectors.has(vectorKey)) {
                continue;
            }
            uniqueVectors.add(vectorKey);
            for (const key of objectiveKeys) {
                const value = solution[key];
                if (typeof value === "number") {
                    aggregatedTargets[key] += value;
                }
            }
            // Strip metadata so each Pareto vertex only contains value-bearing fields.
            const { feasible: _feasible, result: _result, bounded: _bounded, ...paretoPayload } = solution;
            paretoVertices.push(paretoPayload);
        }
        // Derive equality constraints that represent the averaged objective values.
        for (const key of objectiveKeys) {
            workingModel.constraints[key] = { equal: aggregatedTargets[key] / uniqueVectors.size };
        }
        // Add a synthetic objective so the solver has something concrete to maximize.
        const syntheticObjective = `cheater-${Math.random()}`;
        workingModel.optimize = syntheticObjective;
        workingModel.opType = "max";
        for (const variable of Object.values(workingModel.variables)) {
            variable[syntheticObjective] = 1;
        }
        const ranges = computeRanges(paretoVertices);
        const midpoint = asPolyoptSolution(solver.Solve(workingModel, undefined, undefined, true));
        return {
            midpoint,
            vertices: paretoVertices,
            ranges,
        };
    }

    function getDefaultExportFromCjs (x) {
    	return x && x.__esModule && Object.prototype.hasOwnProperty.call(x, 'default') ? x['default'] : x;
    }

    /**
     * @file src/external/lpsolve/reformat.js
     * @description LP format conversion utilities
     *
     * Converts between jsLPSolver's JSON model format and the standard
     * LP file format used by lp_solve and other solvers:
     * - to_JSON: Parse LP text format into JSON model
     * - to_lpsolve: Convert JSON model to LP text format
     *
     * Supports objectives, constraints, integer/binary variables.
     */

    /*global describe*/
    /*global require*/
    /*global module*/
    /*global it*/
    /*global console*/
    /*global process*/
    /*jshint -W083 */
    function to_JSON(input) {
        var rxo = {
                /* jshint ignore:start */
                is_blank: /^\W{0,}$/,
                is_objective: /(max|min)(imize){0,}\:/i,
                is_int: /^(?!\/\*)\W{0,}int/i,
                is_bin: /^(?!\/\*)\W{0,}bin/i,
                is_constraint: /(\>|\<){0,}\=/i,
                is_unrestricted: /^\S{0,}unrestricted/i,
                parse_lhs: /(\-|\+){0,1}\s{0,1}\d{0,}\.{0,}\d{0,}\s{0,}[A-Za-z]\S{0,}/gi,
                parse_rhs: /(\-|\+){0,1}\d{1,}\.{0,}\d{0,}\W{0,}\;{0,1}$/i,
                parse_dir: /(\>|\<){0,}\=/gi,
                parse_int: /[^\s|^\,]+/gi,
                parse_bin: /[^\s|^\,]+/gi,
                get_num: /(\-|\+){0,1}(\W|^)\d+\.{0,1}\d{0,}/g,
                get_word: /[A-Za-z].*/,
                /* jshint ignore:end */
            },
            model = {
                opType: "",
                optimize: "_obj",
                constraints: {},
                variables: {},
            },
            constraints = {
                ">=": "min",
                "<=": "max",
                "=": "equal",
            },
            tmp = "",
            ary = null,
            hldr = "",
            hldr2 = "",
            constraint = "",
            rhs = 0;

        // Handle input if its coming
        // to us as a hard string
        // instead of as an array of
        // strings
        if (typeof input === "string") {
            input = input.split("\n");
        }

        // Start iterating over the rows
        // to see what all we have
        for (var i = 0; i < input.length; i++) {
            constraint = "__" + i;

            // Get the string we're working with
            tmp = input[i];

            // Reset the array
            ary = null;

            // Test to see if we're the objective
            if (rxo.is_objective.test(tmp)) {
                // Set up in model the opType
                model.opType = tmp.match(/(max|min)/gi)[0];

                // Pull apart lhs
                ary = tmp
                    .match(rxo.parse_lhs)
                    .map(function (d) {
                        return d.replace(/\s+/, "");
                    })
                    .slice(1);

                // *** STEP 1 *** ///
                // Get the variables out
                ary.forEach(function (d) {
                    // Get the number if its there
                    hldr = d.match(rxo.get_num);

                    // If it isn't a number, it might
                    // be a standalone variable
                    if (hldr === null) {
                        if (d.substr(0, 1) === "-") {
                            hldr = -1;
                        } else {
                            hldr = 1;
                        }
                    } else {
                        hldr = hldr[0];
                    }

                    hldr = parseFloat(hldr);

                    // Get the variable type
                    hldr2 = d.match(rxo.get_word)[0].replace(/\;$/, "");

                    // Make sure the variable is in the model
                    model.variables[hldr2] = model.variables[hldr2] || {};
                    model.variables[hldr2]._obj = hldr;
                });
                ////////////////////////////////////
            } else if (rxo.is_int.test(tmp)) {
                // Get the array of ints
                ary = tmp.match(rxo.parse_int).slice(1);

                // Since we have an int, our model should too
                model.ints = model.ints || {};

                ary.forEach(function (d) {
                    d = d.replace(";", "");
                    model.ints[d] = 1;
                });
                ////////////////////////////////////
            } else if (rxo.is_bin.test(tmp)) {
                // Get the array of bins
                ary = tmp.match(rxo.parse_bin).slice(1);

                // Since we have an binary, our model should too
                model.binaries = model.binaries || {};

                ary.forEach(function (d) {
                    d = d.replace(";", "");
                    model.binaries[d] = 1;
                });
                ////////////////////////////////////
            } else if (rxo.is_constraint.test(tmp)) {
                var separatorIndex = tmp.indexOf(":");
                var constraintExpression = separatorIndex === -1 ? tmp : tmp.slice(separatorIndex + 1);

                // Pull apart lhs
                ary = constraintExpression.match(rxo.parse_lhs).map(function (d) {
                    return d.replace(/\s+/, "");
                });

                // *** STEP 1 *** ///
                // Get the variables out
                ary.forEach(function (d) {
                    // Get the number if its there
                    hldr = d.match(rxo.get_num);

                    if (hldr === null) {
                        if (d.substr(0, 1) === "-") {
                            hldr = -1;
                        } else {
                            hldr = 1;
                        }
                    } else {
                        hldr = hldr[0];
                    }

                    hldr = parseFloat(hldr);

                    // Get the variable name
                    hldr2 = d.match(rxo.get_word)[0];

                    // Make sure the variable is in the model
                    model.variables[hldr2] = model.variables[hldr2] || {};
                    model.variables[hldr2][constraint] = hldr;
                });

                // *** STEP 2 *** ///
                // Get the RHS out
                rhs = parseFloat(tmp.match(rxo.parse_rhs)[0]);

                // *** STEP 3 *** ///
                // Get the Constrainer out
                tmp = constraints[tmp.match(rxo.parse_dir)[0]];
                model.constraints[constraint] = model.constraints[constraint] || {};
                model.constraints[constraint][tmp] = rhs;
                ////////////////////////////////////
            } else if (rxo.is_unrestricted.test(tmp)) {
                // Get the array of unrestricted
                ary = tmp.match(rxo.parse_int).slice(1);

                // Since we have an int, our model should too
                model.unrestricted = model.unrestricted || {};

                ary.forEach(function (d) {
                    d = d.replace(";", "");
                    model.unrestricted[d] = 1;
                });
            }
        }
        return model;
    }

    /*************************************************************
     * Method: from_JSON
     * Scope: Public:
     * Agruments: model: The model we want solver to operate on
     * Purpose: Convert a friendly JSON model into a model for a
     *          real solving library...in this case
     *          lp_solver
     **************************************************************/
    function from_JSON(model) {
        // Make sure we at least have a model
        if (!model) {
            throw new Error("Solver requires a model to operate on");
        }

        var output = "",
            lookup = {
                max: "<=",
                min: ">=",
                equal: "=",
            },
            rxClean = new RegExp("[^A-Za-z0-9_\[\{\}\/\.\&\#\$\%\~\'\@\^]", "gi");

        // Build the objective statement

        if (model.opType) {
            output += model.opType + ":";

            // Iterate over the variables
            for (var x in model.variables) {
                // Give each variable a self of 1 unless
                // it exists already
                model.variables[x][x] = model.variables[x][x] ? model.variables[x][x] : 1;

                // Does our objective exist here?
                if (model.variables[x][model.optimize]) {
                    output += " " + model.variables[x][model.optimize] + " " + x.replace(rxClean, "_");
                }
            }
        } else {
            output += "max:";
        }

        // Add some closure to our line thing
        output += ";\n\n";

        // And now... to iterate over the constraints
        for (var xx in model.constraints) {
            for (var y in model.constraints[xx]) {
                if (typeof lookup[y] !== "undefined") {
                    for (var z in model.variables) {
                        // Does our Constraint exist here?
                        if (typeof model.variables[z][xx] !== "undefined") {
                            output += " " + model.variables[z][xx] + " " + z.replace(rxClean, "_");
                        }
                    }
                    // Add the constraint type and value...

                    output += " " + lookup[y] + " " + model.constraints[xx][y];
                    output += ";\n";
                }
            }
        }

        // Are there any ints?
        if (model.ints) {
            output += "\n\n";
            for (var xxx in model.ints) {
                output += "int " + xxx.replace(rxClean, "_") + ";\n";
            }
        }

        // Are there any unrestricted?
        if (model.unrestricted) {
            output += "\n\n";
            for (var xxxx in model.unrestricted) {
                output += "unrestricted " + xxxx.replace(rxClean, "_") + ";\n";
            }
        }

        // And kick the string back
        return output;
    }

    var reformat = function (model) {
        // If the user is giving us an array
        // or a string, convert it to a JSON Model
        // otherwise, spit it out as a string
        if (model.length) {
            return to_JSON(model);
        } else {
            return from_JSON(model);
        }
    };

    var ReformatLP = /*@__PURE__*/getDefaultExportFromCjs(reformat);

    function createCut$1(type, varIndex, value) {
        return { type, varIndex, value };
    }
    function createBranch$1(relaxedEvaluation, cuts, depth, branchVarIndex, branchDirection, branchFractionality, parentEvaluation) {
        return {
            relaxedEvaluation,
            cuts,
            depth,
            branchVarIndex,
            branchDirection,
            branchFractionality,
            parentEvaluation,
        };
    }
    /**
     * Enhanced branch-and-cut with:
     * - Pseudocost branching
     * - Hybrid node selection (depth-first early, best-first later)
     * - Diving heuristic for quick feasible solutions
     */
    function createEnhancedBranchAndCutService(options = {}) {
        const { nodeSelection = "hybrid", branching = "pseudocost", useDiving: _useDiving = true, // Reserved for future diving heuristic
        strongBranchingCandidates = 5, } = options;
        // Pseudocost data per variable index
        const pseudoCosts = new Map();
        const getPseudoCost = (varIndex) => {
            let data = pseudoCosts.get(varIndex);
            if (!data) {
                data = { upSum: 0, upCount: 0, downSum: 0, downCount: 0 };
                pseudoCosts.set(varIndex, data);
            }
            return data;
        };
        const updatePseudoCost = (varIndex, direction, improvement, fraction) => {
            const data = getPseudoCost(varIndex);
            const normalizedImprovement = improvement / (direction === "up" ? 1 - fraction : fraction);
            if (direction === "up") {
                data.upSum += normalizedImprovement;
                data.upCount++;
            }
            else {
                data.downSum += normalizedImprovement;
                data.downCount++;
            }
        };
        const getScore = (varIndex, fraction) => {
            const data = getPseudoCost(varIndex);
            // Use geometric mean of up and down pseudocosts
            const upPseudo = data.upCount > 0 ? data.upSum / data.upCount : 1;
            const downPseudo = data.downCount > 0 ? data.downSum / data.downCount : 1;
            const upEstimate = upPseudo * (1 - fraction);
            const downEstimate = downPseudo * fraction;
            // Product score (like SCIP's default)
            return Math.max(upEstimate, 1e-6) * Math.max(downEstimate, 1e-6);
        };
        const selectBranchingVariable = (tableau, _currentEval) => {
            const width = tableau.width;
            const matrix = tableau.matrix;
            const rhsColumn = tableau.rhsColumn;
            const integerVars = tableau.model.integerVariables;
            const precision = tableau.precision;
            let candidates = [];
            // Collect fractional variables
            for (const variable of integerVars) {
                const varIndex = variable.index;
                const row = tableau.rowByVarIndex[varIndex];
                if (row !== -1) {
                    const value = matrix[row * width + rhsColumn];
                    const fraction = Math.abs(value - Math.round(value));
                    if (fraction > precision) {
                        candidates.push({ index: varIndex, value, fraction });
                    }
                }
            }
            if (candidates.length === 0)
                return null;
            if (branching === "most-fractional") {
                // Original strategy - pick most fractional
                candidates.sort((a, b) => b.fraction - a.fraction);
                return { index: candidates[0].index, value: candidates[0].value };
            }
            if (branching === "pseudocost") {
                // Score by pseudocosts
                let bestScore = -Infinity;
                let bestCandidate = candidates[0];
                for (const candidate of candidates) {
                    const score = getScore(candidate.index, candidate.fraction);
                    if (score > bestScore) {
                        bestScore = score;
                        bestCandidate = candidate;
                    }
                }
                return { index: bestCandidate.index, value: bestCandidate.value };
            }
            if (branching === "strong") {
                // Strong branching on top candidates
                // Sort by most fractional first
                candidates.sort((a, b) => b.fraction - a.fraction);
                candidates = candidates.slice(0, strongBranchingCandidates);
                let bestScore = -Infinity;
                let bestCandidate = candidates[0];
                // For strong branching, we'd solve LP relaxations
                // Here we use a simplified version with pseudocost estimation
                for (const candidate of candidates) {
                    const data = getPseudoCost(candidate.index);
                    // If we have enough pseudocost data, use it
                    if (data.upCount >= 2 && data.downCount >= 2) {
                        const score = getScore(candidate.index, candidate.fraction);
                        if (score > bestScore) {
                            bestScore = score;
                            bestCandidate = candidate;
                        }
                    }
                    else {
                        // Fall back to most fractional
                        const score = candidate.fraction * (1 - candidate.fraction);
                        if (score > bestScore) {
                            bestScore = score;
                            bestCandidate = candidate;
                        }
                    }
                }
                return { index: bestCandidate.index, value: bestCandidate.value };
            }
            return { index: candidates[0].index, value: candidates[0].value };
        };
        const applyCuts = (tableau, branchingCuts) => {
            var _a;
            tableau.restore();
            tableau.addCutConstraints(branchingCuts);
            tableau.simplex();
            if (((_a = tableau.model) === null || _a === void 0 ? void 0 : _a.useMIRCuts) && tableau.feasible) {
                let fractionalVolumeImproved = true;
                let mirIterations = 0;
                const maxMIRIterations = 3;
                while (fractionalVolumeImproved && mirIterations < maxMIRIterations) {
                    const fractionalVolumeBefore = tableau.computeFractionalVolume(true);
                    tableau.applyMIRCuts();
                    tableau.simplex();
                    const fractionalVolumeAfter = tableau.computeFractionalVolume(true);
                    mirIterations++;
                    if (fractionalVolumeAfter >= 0.9 * fractionalVolumeBefore) {
                        fractionalVolumeImproved = false;
                    }
                }
            }
        };
        const branchAndCut = (tableau) => {
            var _a, _b, _c, _d, _e;
            const branches = new BranchMinHeap();
            const depthFirstStack = [];
            let iterations = 0;
            const tolerance = (_b = (_a = tableau.model) === null || _a === void 0 ? void 0 : _a.tolerance) !== null && _b !== void 0 ? _b : 0;
            let toleranceFlag = true;
            let terminalTime = 1e99;
            if ((_c = tableau.model) === null || _c === void 0 ? void 0 : _c.timeout) {
                terminalTime = Date.now() + tableau.model.timeout;
            }
            let bestEvaluation = Infinity;
            let bestBranch = null;
            const bestOptionalObjectivesEvaluations = [];
            for (let oInit = 0; oInit < tableau.optionalObjectives.length; oInit += 1) {
                bestOptionalObjectivesEvaluations.push(Infinity);
            }
            // Configuration for hybrid node selection
            const switchTobestFirstAfterSolutions = 1;
            let solutionsFound = 0;
            let useDepthFirst = nodeSelection === "depth-first" || nodeSelection === "hybrid";
            const branch = createBranch$1(-Infinity, [], 0);
            let acceptableThreshold;
            if (useDepthFirst) {
                depthFirstStack.push(branch);
            }
            else {
                branches.push(branch);
            }
            while ((useDepthFirst ? depthFirstStack.length > 0 : !branches.isEmpty()) &&
                toleranceFlag === true &&
                Date.now() < terminalTime) {
                if ((_d = tableau.model) === null || _d === void 0 ? void 0 : _d.isMinimization) {
                    acceptableThreshold = tableau.bestPossibleEval * (1 + tolerance);
                }
                else {
                    acceptableThreshold = tableau.bestPossibleEval * (1 - tolerance);
                }
                if (tolerance > 0 && bestEvaluation < acceptableThreshold) {
                    toleranceFlag = false;
                }
                // Select next node based on strategy
                let activeBranch;
                if (useDepthFirst && depthFirstStack.length > 0) {
                    activeBranch = depthFirstStack.pop();
                }
                else if (!branches.isEmpty()) {
                    activeBranch = branches.pop();
                }
                else {
                    break;
                }
                if (activeBranch.relaxedEvaluation >= bestEvaluation) {
                    continue;
                }
                const cuts = activeBranch.cuts;
                tableau.evaluation;
                applyCuts(tableau, cuts);
                iterations++;
                if (!tableau.feasible) {
                    continue;
                }
                const evaluation = tableau.evaluation;
                if (evaluation > bestEvaluation) {
                    continue;
                }
                // Update pseudocosts based on observed improvement
                if (activeBranch.branchVarIndex !== undefined &&
                    activeBranch.branchDirection !== undefined &&
                    activeBranch.branchFractionality !== undefined &&
                    activeBranch.parentEvaluation !== undefined) {
                    const improvement = Math.abs(evaluation - activeBranch.parentEvaluation);
                    updatePseudoCost(activeBranch.branchVarIndex, activeBranch.branchDirection, improvement, activeBranch.branchFractionality);
                }
                if (evaluation === bestEvaluation) {
                    let isCurrentEvaluationWorse = true;
                    for (let o = 0; o < tableau.optionalObjectives.length; o++) {
                        if (tableau.optionalObjectives[o].reducedCosts[0] >
                            bestOptionalObjectivesEvaluations[o]) {
                            break;
                        }
                        else if (tableau.optionalObjectives[o].reducedCosts[0] <
                            bestOptionalObjectivesEvaluations[o]) {
                            isCurrentEvaluationWorse = false;
                            break;
                        }
                    }
                    if (isCurrentEvaluationWorse) {
                        continue;
                    }
                }
                if (tableau.isIntegral()) {
                    tableau.__isIntegral = true;
                    solutionsFound++;
                    if (iterations === 1) {
                        tableau.branchAndCutIterations = iterations;
                        return;
                    }
                    bestBranch = activeBranch;
                    bestEvaluation = evaluation;
                    for (let oCopy = 0; oCopy < tableau.optionalObjectives.length; oCopy++) {
                        bestOptionalObjectivesEvaluations[oCopy] =
                            tableau.optionalObjectives[oCopy].reducedCosts[0];
                    }
                    if ((_e = tableau.model) === null || _e === void 0 ? void 0 : _e.keep_solutions) {
                        const nowSolution = tableau.model.tableau.getSolution();
                        const store = nowSolution.generateSolutionSet();
                        store.result = nowSolution.evaluation;
                        if (!tableau.model.solutions) {
                            tableau.model.solutions = [];
                        }
                        tableau.model.solutions.push(store);
                    }
                    // Switch to best-first after finding solutions
                    if (nodeSelection === "hybrid" &&
                        solutionsFound >= switchTobestFirstAfterSolutions) {
                        useDepthFirst = false;
                        // Move remaining depth-first nodes to priority queue
                        while (depthFirstStack.length > 0) {
                            branches.push(depthFirstStack.pop());
                        }
                    }
                }
                else {
                    if (iterations === 1) {
                        tableau.save();
                    }
                    // Use enhanced branching variable selection
                    const variable = selectBranchingVariable(tableau);
                    if (!variable)
                        continue;
                    const varIndex = variable.index;
                    const varValue = variable.value;
                    const cutsHigh = [];
                    const cutsLow = [];
                    const nCuts = cuts.length;
                    for (let c = 0; c < nCuts; c++) {
                        const cut = cuts[c];
                        if (cut.varIndex === varIndex) {
                            if (cut.type === "min") {
                                cutsLow.push(cut);
                            }
                            else {
                                cutsHigh.push(cut);
                            }
                        }
                        else {
                            cutsHigh.push(cut);
                            cutsLow.push(cut);
                        }
                    }
                    const ceilVal = Math.ceil(varValue);
                    const floorVal = Math.floor(varValue);
                    const fracUp = ceilVal - varValue; // Distance to ceil
                    const fracDown = varValue - floorVal; // Distance to floor
                    const cutHigh = createCut$1("min", varIndex, ceilVal);
                    cutsHigh.push(cutHigh);
                    const cutLow = createCut$1("max", varIndex, floorVal);
                    cutsLow.push(cutLow);
                    const newDepth = activeBranch.depth + 1;
                    if (useDepthFirst) {
                        // Push in reverse order so 'up' branch is explored first
                        // (often better for minimization with binary vars)
                        depthFirstStack.push(createBranch$1(evaluation, cutsLow, newDepth, varIndex, "down", fracDown, evaluation));
                        depthFirstStack.push(createBranch$1(evaluation, cutsHigh, newDepth, varIndex, "up", fracUp, evaluation));
                    }
                    else {
                        branches.push(createBranch$1(evaluation, cutsHigh, newDepth, varIndex, "up", fracUp, evaluation));
                        branches.push(createBranch$1(evaluation, cutsLow, newDepth, varIndex, "down", fracDown, evaluation));
                    }
                }
            }
            if (bestBranch !== null) {
                applyCuts(tableau, bestBranch.cuts);
            }
            tableau.branchAndCutIterations = iterations;
        };
        return { applyCuts, branchAndCut };
    }

    function createCheckpoint(tableau) {
        return {
            matrix: new Float64Array(tableau.matrix),
            width: tableau.width,
            height: tableau.height,
            nVars: tableau.nVars,
            varIndexByRow: tableau.varIndexByRow.slice(),
            varIndexByCol: tableau.varIndexByCol.slice(),
            rowByVarIndex: tableau.rowByVarIndex.slice(),
            colByVarIndex: tableau.colByVarIndex.slice(),
            availableIndexes: tableau.availableIndexes.slice(),
            lastElementIndex: tableau.lastElementIndex,
            evaluation: tableau.evaluation,
            feasible: tableau.feasible,
        };
    }
    function restoreCheckpoint(tableau, checkpoint) {
        // Only copy if sizes match, otherwise need full restore
        if (tableau.matrix.length >= checkpoint.matrix.length) {
            tableau.matrix.set(checkpoint.matrix);
        }
        else {
            tableau.matrix = new Float64Array(checkpoint.matrix);
        }
        tableau.width = checkpoint.width;
        tableau.height = checkpoint.height;
        tableau.nVars = checkpoint.nVars;
        // Restore arrays
        const height = checkpoint.height;
        for (let i = 0; i < height; i++) {
            tableau.varIndexByRow[i] = checkpoint.varIndexByRow[i];
        }
        tableau.varIndexByRow.length = height;
        const width = checkpoint.width;
        for (let i = 0; i < width; i++) {
            tableau.varIndexByCol[i] = checkpoint.varIndexByCol[i];
        }
        tableau.varIndexByCol.length = width;
        const nVars = checkpoint.nVars;
        for (let i = 0; i < nVars; i++) {
            tableau.rowByVarIndex[i] = checkpoint.rowByVarIndex[i];
            tableau.colByVarIndex[i] = checkpoint.colByVarIndex[i];
        }
        tableau.availableIndexes = checkpoint.availableIndexes.slice();
        tableau.lastElementIndex = checkpoint.lastElementIndex;
        tableau.evaluation = checkpoint.evaluation;
        tableau.feasible = checkpoint.feasible;
    }
    function createCut(type, varIndex, value) {
        return { type, varIndex, value };
    }
    function createBranch(relaxedEvaluation, cuts, depth, parentCheckpoint, newCut) {
        return { relaxedEvaluation, cuts, depth, parentCheckpoint, newCut };
    }
    function createIncrementalBranchAndCutService(options = {}) {
        const { nodeSelection = "hybrid", branching = "pseudocost", maxCheckpoints = 50, // Limit checkpoints to avoid memory overhead
         } = options;
        const pseudoCosts = new Map();
        const getPseudoCost = (varIndex) => {
            let data = pseudoCosts.get(varIndex);
            if (!data) {
                data = { upSum: 0, upCount: 0, downSum: 0, downCount: 0 };
                pseudoCosts.set(varIndex, data);
            }
            return data;
        };
        const updatePseudoCost = (varIndex, direction, improvement, fraction) => {
            const data = getPseudoCost(varIndex);
            const normalizedImprovement = improvement / (direction === "up" ? 1 - fraction : fraction);
            if (direction === "up") {
                data.upSum += normalizedImprovement;
                data.upCount++;
            }
            else {
                data.downSum += normalizedImprovement;
                data.downCount++;
            }
        };
        const getScore = (varIndex, fraction) => {
            const data = getPseudoCost(varIndex);
            const upPseudo = data.upCount > 0 ? data.upSum / data.upCount : 1;
            const downPseudo = data.downCount > 0 ? data.downSum / data.downCount : 1;
            const upEstimate = upPseudo * (1 - fraction);
            const downEstimate = downPseudo * fraction;
            return Math.max(upEstimate, 1e-6) * Math.max(downEstimate, 1e-6);
        };
        const selectBranchingVariable = (tableau) => {
            const width = tableau.width;
            const matrix = tableau.matrix;
            const rhsColumn = tableau.rhsColumn;
            const integerVars = tableau.model.integerVariables;
            const precision = tableau.precision;
            const candidates = [];
            for (const variable of integerVars) {
                const varIndex = variable.index;
                const row = tableau.rowByVarIndex[varIndex];
                if (row !== -1) {
                    const value = matrix[row * width + rhsColumn];
                    const fraction = Math.abs(value - Math.round(value));
                    if (fraction > precision) {
                        candidates.push({ index: varIndex, value, fraction });
                    }
                }
            }
            if (candidates.length === 0)
                return null;
            if (branching === "most-fractional") {
                candidates.sort((a, b) => b.fraction - a.fraction);
                return candidates[0];
            }
            // Pseudocost branching
            let bestScore = -Infinity;
            let bestCandidate = candidates[0];
            for (const candidate of candidates) {
                const score = getScore(candidate.index, candidate.fraction);
                if (score > bestScore) {
                    bestScore = score;
                    bestCandidate = candidate;
                }
            }
            return bestCandidate;
        };
        // Standard applyCuts - restores to root and applies all cuts
        const applyCuts = (tableau, branchingCuts) => {
            var _a;
            tableau.restore();
            tableau.addCutConstraints(branchingCuts);
            tableau.simplex();
            if (((_a = tableau.model) === null || _a === void 0 ? void 0 : _a.useMIRCuts) && tableau.feasible) {
                let fractionalVolumeImproved = true;
                let mirIterations = 0;
                const maxMIRIterations = 3;
                while (fractionalVolumeImproved && mirIterations < maxMIRIterations) {
                    const fractionalVolumeBefore = tableau.computeFractionalVolume(true);
                    tableau.applyMIRCuts();
                    tableau.simplex();
                    const fractionalVolumeAfter = tableau.computeFractionalVolume(true);
                    mirIterations++;
                    if (fractionalVolumeAfter >= 0.9 * fractionalVolumeBefore) {
                        fractionalVolumeImproved = false;
                    }
                }
            }
        };
        // Incremental applyCuts - uses parent checkpoint if available
        const applyIncrementalCuts = (tableau, branch) => {
            var _a;
            if (branch.parentCheckpoint && branch.newCut) {
                // Fast path: restore parent and apply only new cut
                restoreCheckpoint(tableau, branch.parentCheckpoint);
                tableau.addCutConstraints([branch.newCut]);
                tableau.simplex();
            }
            else {
                // Fallback: full restore from root
                tableau.restore();
                tableau.addCutConstraints(branch.cuts);
                tableau.simplex();
            }
            if (((_a = tableau.model) === null || _a === void 0 ? void 0 : _a.useMIRCuts) && tableau.feasible) {
                let fractionalVolumeImproved = true;
                let mirIterations = 0;
                const maxMIRIterations = 3;
                while (fractionalVolumeImproved && mirIterations < maxMIRIterations) {
                    const fractionalVolumeBefore = tableau.computeFractionalVolume(true);
                    tableau.applyMIRCuts();
                    tableau.simplex();
                    const fractionalVolumeAfter = tableau.computeFractionalVolume(true);
                    mirIterations++;
                    if (fractionalVolumeAfter >= 0.9 * fractionalVolumeBefore) {
                        fractionalVolumeImproved = false;
                    }
                }
            }
        };
        const branchAndCut = (tableau) => {
            var _a, _b, _c, _d, _e;
            const branches = new BranchMinHeap();
            const depthFirstStack = [];
            let iterations = 0;
            let checkpointCount = 0;
            const tolerance = (_b = (_a = tableau.model) === null || _a === void 0 ? void 0 : _a.tolerance) !== null && _b !== void 0 ? _b : 0;
            let toleranceFlag = true;
            let terminalTime = 1e99;
            if ((_c = tableau.model) === null || _c === void 0 ? void 0 : _c.timeout) {
                terminalTime = Date.now() + tableau.model.timeout;
            }
            let bestEvaluation = Infinity;
            let bestBranch = null;
            const bestOptionalObjectivesEvaluations = [];
            for (let oInit = 0; oInit < tableau.optionalObjectives.length; oInit++) {
                bestOptionalObjectivesEvaluations.push(Infinity);
            }
            const switchToBestFirstAfterSolutions = 1;
            let solutionsFound = 0;
            let useDepthFirst = nodeSelection === "depth-first" || nodeSelection === "hybrid";
            const rootBranch = createBranch(-Infinity, [], 0);
            if (useDepthFirst) {
                depthFirstStack.push(rootBranch);
            }
            else {
                branches.push(rootBranch);
            }
            while ((useDepthFirst ? depthFirstStack.length > 0 : !branches.isEmpty()) &&
                toleranceFlag === true &&
                Date.now() < terminalTime) {
                let acceptableThreshold;
                if ((_d = tableau.model) === null || _d === void 0 ? void 0 : _d.isMinimization) {
                    acceptableThreshold = tableau.bestPossibleEval * (1 + tolerance);
                }
                else {
                    acceptableThreshold = tableau.bestPossibleEval * (1 - tolerance);
                }
                if (tolerance > 0 && bestEvaluation < acceptableThreshold) {
                    toleranceFlag = false;
                }
                let activeBranch;
                if (useDepthFirst && depthFirstStack.length > 0) {
                    activeBranch = depthFirstStack.pop();
                }
                else if (!branches.isEmpty()) {
                    activeBranch = branches.pop();
                }
                else {
                    break;
                }
                if (activeBranch.relaxedEvaluation >= bestEvaluation) {
                    continue;
                }
                const parentEval = tableau.evaluation;
                // Use incremental restoration if available
                applyIncrementalCuts(tableau, activeBranch);
                iterations++;
                if (!tableau.feasible) {
                    continue;
                }
                const evaluation = tableau.evaluation;
                if (evaluation > bestEvaluation) {
                    continue;
                }
                // Update pseudocosts
                if (activeBranch.newCut && parentEval !== 0) {
                    const improvement = Math.abs(evaluation - parentEval);
                    const fraction = 0.5;
                    updatePseudoCost(activeBranch.newCut.varIndex, activeBranch.newCut.type === "min" ? "up" : "down", improvement, fraction);
                }
                if (evaluation === bestEvaluation) {
                    let isCurrentEvaluationWorse = true;
                    for (let o = 0; o < tableau.optionalObjectives.length; o++) {
                        if (tableau.optionalObjectives[o].reducedCosts[0] >
                            bestOptionalObjectivesEvaluations[o]) {
                            break;
                        }
                        else if (tableau.optionalObjectives[o].reducedCosts[0] <
                            bestOptionalObjectivesEvaluations[o]) {
                            isCurrentEvaluationWorse = false;
                            break;
                        }
                    }
                    if (isCurrentEvaluationWorse) {
                        continue;
                    }
                }
                if (tableau.isIntegral()) {
                    tableau.__isIntegral = true;
                    solutionsFound++;
                    if (iterations === 1) {
                        tableau.branchAndCutIterations = iterations;
                        return;
                    }
                    bestBranch = activeBranch;
                    bestEvaluation = evaluation;
                    for (let oCopy = 0; oCopy < tableau.optionalObjectives.length; oCopy++) {
                        bestOptionalObjectivesEvaluations[oCopy] =
                            tableau.optionalObjectives[oCopy].reducedCosts[0];
                    }
                    if ((_e = tableau.model) === null || _e === void 0 ? void 0 : _e.keep_solutions) {
                        const nowSolution = tableau.model.tableau.getSolution();
                        const store = nowSolution.generateSolutionSet();
                        store.result = nowSolution.evaluation;
                        if (!tableau.model.solutions) {
                            tableau.model.solutions = [];
                        }
                        tableau.model.solutions.push(store);
                    }
                    // Switch to best-first after finding solutions
                    if (nodeSelection === "hybrid" &&
                        solutionsFound >= switchToBestFirstAfterSolutions) {
                        useDepthFirst = false;
                        while (depthFirstStack.length > 0) {
                            branches.push(depthFirstStack.pop());
                        }
                    }
                }
                else {
                    if (iterations === 1) {
                        tableau.save();
                    }
                    const variable = selectBranchingVariable(tableau);
                    if (!variable)
                        continue;
                    const varIndex = variable.index;
                    const varValue = variable.value;
                    // Create checkpoint for children (only if under limit)
                    let checkpoint;
                    if (useDepthFirst && checkpointCount < maxCheckpoints) {
                        checkpoint = createCheckpoint(tableau);
                        checkpointCount++;
                    }
                    const cutsHigh = [];
                    const cutsLow = [];
                    const nCuts = activeBranch.cuts.length;
                    for (let c = 0; c < nCuts; c++) {
                        const cut = activeBranch.cuts[c];
                        if (cut.varIndex === varIndex) {
                            if (cut.type === "min") {
                                cutsLow.push(cut);
                            }
                            else {
                                cutsHigh.push(cut);
                            }
                        }
                        else {
                            cutsHigh.push(cut);
                            cutsLow.push(cut);
                        }
                    }
                    const min = Math.ceil(varValue);
                    const max = Math.floor(varValue);
                    const cutHigh = createCut("min", varIndex, min);
                    cutsHigh.push(cutHigh);
                    const cutLow = createCut("max", varIndex, max);
                    cutsLow.push(cutLow);
                    const newDepth = activeBranch.depth + 1;
                    if (useDepthFirst) {
                        // Push with parent checkpoint for incremental restoration
                        depthFirstStack.push(createBranch(evaluation, cutsLow, newDepth, checkpoint, cutLow));
                        depthFirstStack.push(createBranch(evaluation, cutsHigh, newDepth, checkpoint, cutHigh));
                    }
                    else {
                        // Best-first doesn't use checkpoints (would need too much memory)
                        branches.push(createBranch(evaluation, cutsHigh, newDepth));
                        branches.push(createBranch(evaluation, cutsLow, newDepth));
                    }
                }
            }
            if (bestBranch !== null) {
                applyCuts(tableau, bestBranch.cuts);
            }
            tableau.branchAndCutIterations = iterations;
        };
        return { applyCuts, branchAndCut };
    }

    /**
     * @file src/main.ts
     * @description Core Solver class implementation
     *
     * Orchestrates the complete solving pipeline:
     * - Model parsing and validation
     * - Simplex algorithm for linear programming
     * - Branch-and-cut for mixed-integer programming
     * - Multi-objective optimization via Polyopt
     * - External solver delegation (e.g., lp_solve)
     */
    /**
     * Main solver class providing the public API for solving optimization problems.
     */
    class Solver {
        constructor() {
            // Expose constructors for programmatic model building
            this.Model = Model;
            this.Tableau = Tableau;
            this.Constraint = Constraint;
            this.Variable = Variable;
            this.Numeral = Numeral;
            this.Term = Term;
            // External solver integrations
            this.External = External;
            this.ReformatLP = ReformatLP;
            // Branch-and-cut service (default implementation)
            this.branchAndCutService = createBranchAndCutService();
            this.branchAndCut = (tableau) => this.branchAndCutService.branchAndCut(tableau);
            // Reference to the last solved model (useful for debugging)
            this.lastSolvedModel = null;
        }
        /**
         * Select the appropriate branch-and-cut service based on model options.
         *
         * Enhanced strategies can be enabled via model.options:
         * - nodeSelection: 'best-first' | 'depth-first' | 'hybrid'
         * - branching: 'most-fractional' | 'pseudocost' | 'strong'
         * - useIncremental: true to use incremental state management (experimental)
         */
        selectBranchAndCutService(model) {
            var _a, _b, _c, _d;
            const options = model.options;
            const useEnhanced = (options === null || options === void 0 ? void 0 : options.nodeSelection) || (options === null || options === void 0 ? void 0 : options.branching);
            const useIncremental = (options === null || options === void 0 ? void 0 : options.useIncremental) === true; // Must explicitly enable
            if (useIncremental) {
                return createIncrementalBranchAndCutService({
                    nodeSelection: (_a = options === null || options === void 0 ? void 0 : options.nodeSelection) !== null && _a !== void 0 ? _a : "hybrid",
                    branching: (_b = options === null || options === void 0 ? void 0 : options.branching) !== null && _b !== void 0 ? _b : "pseudocost",
                });
            }
            if (useEnhanced) {
                return createEnhancedBranchAndCutService({
                    nodeSelection: (_c = options === null || options === void 0 ? void 0 : options.nodeSelection) !== null && _c !== void 0 ? _c : "hybrid",
                    branching: (_d = options === null || options === void 0 ? void 0 : options.branching) !== null && _d !== void 0 ? _d : "pseudocost",
                    useDiving: true,
                });
            }
            return createBranchAndCutService();
        }
        /**
         * Solve a linear or mixed-integer programming problem.
         *
         * @param model - Problem definition (JSON format or Model instance)
         * @param precision - Tolerance for integer constraints (default: 1e-9)
         * @param full - If true, return full Solution object; otherwise return simplified result
         * @param validate - If true, run model through validation functions
         * @returns Solution object or simplified result with variable values
         */
        Solve(model, precision, full, validate) {
            // Run validation if requested
            if (validate) {
                for (const test in validation) {
                    const validator = validation[test];
                    if (typeof validator === "function") {
                        model = validator(model);
                    }
                }
            }
            if (!model) {
                throw new Error("Solver requires a model to operate on");
            }
            // Handle multi-objective optimization
            if (typeof model.optimize === "object") {
                if (Object.keys(model.optimize).length > 1) {
                    return Polyopt(this, model);
                }
            }
            // Handle external solver delegation
            if (model.external) {
                return this.solveWithExternalSolver(model);
            }
            // Solve with internal solver
            let modelInstance;
            if (!(model instanceof Model)) {
                const branchAndCutService = this.selectBranchAndCutService(model);
                modelInstance = new Model(precision, undefined, branchAndCutService).loadJson(model);
            }
            else {
                modelInstance = model;
            }
            const solution = modelInstance.solve();
            this.lastSolvedModel = modelInstance;
            solution.solutionSet = solution.generateSolutionSet();
            // Return full solution or simplified result
            if (full) {
                return solution;
            }
            return this.buildSimplifiedResult(solution);
        }
        /**
         * Delegate solving to an external solver (e.g., lp_solve).
         */
        solveWithExternalSolver(model) {
            var _a;
            const solvers = Object.keys(External);
            const solverList = JSON.stringify(solvers);
            if (!((_a = model.external) === null || _a === void 0 ? void 0 : _a.solver)) {
                throw new Error(`Model has 'external' object without solver attribute. Available: ${solverList}`);
            }
            const requestedSolver = model.external.solver;
            if (!External[requestedSolver]) {
                throw new Error(`Solver '${requestedSolver}' not supported. Available: ${solverList}`);
            }
            return External[requestedSolver].solve(model);
        }
        /**
         * Build a simplified result object from a full solution.
         */
        buildSimplifiedResult(solution) {
            const result = {
                feasible: solution.feasible,
                result: solution.evaluation,
                bounded: solution.bounded,
            };
            if (solution._tableau.__isIntegral) {
                result.isIntegral = true;
            }
            // Add non-zero variable values
            for (const varId of Object.keys(solution.solutionSet)) {
                const value = solution.solutionSet[varId];
                if (value !== 0) {
                    result[varId] = value;
                }
            }
            return result;
        }
        /**
         * Solve a multi-objective optimization problem.
         *
         * Returns a compromise solution using the mid-point formula between
         * individually optimized objectives.
         *
         * @example
         * const model = {
         *     optimize: { profit: "max", risk: "min" },
         *     constraints: { budget: { max: 1000 } },
         *     variables: { ... }
         * };
         * const result = solver.MultiObjective(model);
         */
        MultiObjective(model) {
            return Polyopt(this, model);
        }
    }
    // Create singleton instance
    const solver = new Solver();
    // UMD module exports for various environments
    if (typeof define === "function") {
        // AMD (RequireJS)
        define([], () => solver);
    }
    else if (typeof window === "object") {
        // Browser global
        window.solver = solver;
    }
    else if (typeof self === "object") {
        // Web Worker
        self.solver = solver;
    }

    /**
     * @file src/solver.ts
     * @description Main entry point for jsLPSolver library
     *
     * Re-exports the solver instance and all public types. This is the primary
     * import target for library consumers.
     *
     * @example
     * import solver from "javascript-lp-solver";
     * const result = solver.Solve(model);
     */

    return solver;

})();
//# sourceMappingURL=solver.global.js.map
