export class Model {
  constructor() {
    this._states = [];
  }

  addTransition(sourceIdx, targetIdx) {
    if (!this._states[sourceIdx] || !this._states[target]) return;

    let successors = this._states[sourceIdx].successors,
      index = successors.indexOf(targetIdx);
    if (index === -1) successors.push(targetIdx);
  }

  removeTransition(sourceIdx, targetIdx) {
    if (!this._states[sourceIdx]) return;

    let successors = this._states[sourceIdx].successors,
      index = successors.indexOf(targetIdx);
    if (index !== -1) successors.splice(index, 1);
  }

  getSuccessorsOf(sourceIdx) {
    if (!this._states[sourceIdx]) return;

    return this._states[sourceIdx].successors;
  }

  addState(assignment) {
    let processedAssignment = {};
    for (let propvar in assignment)
      if (assignment[propvar] === true)
        processedAssignment[propvar] = assignment[propvar];

    this._states.push({ assignment: processedAssignment, successors: [] });
  }

  editState(stateIdx, assignment) {
    if (!this._states[stateIdx]) return;

    let stateAssignment = this._states[stateIdx].assignment;
    for (let propvar in assignment)
      if (assignment[propvar] === true) stateAssignment[propvar] = true;
      else if (assignment[propvar] === false) delete stateAssignment[propvar];
  }

  removeState(stateIdx) {
    if (!this._states[stateIdx]) return;
    this._states[state] = null;
    this._states.forEach((source, idx) => {
      if (source) this.removeTransition(idx, stateIdx);
    });
  }

  getStates() {
    return this._states.map(state => {
      if (state) return state.assignment;
      else return null;
    });
  }

  valuation(propVar, stateIdx) {
    if (!this._states[stateIdx])
      throw new Error("State " + stateIdx + " not found!");
    return Boolean(this._states[stateIdx].assignment[propVar]);
  }

  getPursForeign() {
    const worlds = [];
    const relation = [];
    const valuation = [];
    const domain = [];
    this._states.forEach((state, idx) => {
      worlds.push(idx + '');
      relation.push(state.successors.map(toIdx => ({
        to: toIdx + '',
        from: idx + ''
      })));
      for (let propvar in state.assignment) {
        if (state.assignment[propvar] === true) {
          valuation.push({
            world: idx + '',
            atom: propvar
          });
          domain.push({
            world: idx + '',
            atom: propvar
          });
        }
      }
    })
    return { worlds, relation, valuation };
  }

  loadFromModelString(modelString) {
    var regex = /^(?:;|(?:A|A(?:\w+,)*\w+)(?:S|S(?:\d+,)*\d+);)+$/;
    if (!regex.test(modelString)) return;

    this._states = [];

    var self = this,
      successorLists = [],
      inputStates = modelString.split(';').slice(0, -1);

    // restore states
    inputStates.forEach(function (state) {
      if (!state) {
        this._states.push(null);
        successorLists.push(null);
        return;
      }

      var stateProperties = state.match(/A(.*)S(.*)/).slice(1, 3)
        .map(function (substr) { return (substr ? substr.split(',') : []); });

      var assignment = {};
      stateProperties[0].forEach(function (propvar) { assignment[propvar] = true; });
      this._states.push({ assignment: assignment, successors: [] });

      var successors = stateProperties[1].map(function (succState) { return +succState; });
      successorLists.push(successors);
    });

    // restore transitions
    successorLists.forEach(function (successors, source) {
      if (!successors) return;

      successors.forEach(function (target) {
        self.addTransition(source, target);
      });
    });
  }

  getModelString() {
    var modelString = '';

    this._states.forEach(function (state) {
      if (state) {
        modelString += 'A' + Object.keys(state.assignment).join();
        modelString += 'S' + state.successors.join();
      }
      modelString += ';';
    });

    return modelString;
  }
}
