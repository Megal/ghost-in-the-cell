// Created by Svyatoshenko "Megal" Misha 2017-02-25

import Foundation

// MARK: - Dot Transform operation
precedencegroup DotOperationPrecedence {
	higherThan: MultiplicationPrecedence
	assignment: true
}
infix operator .--> : DotOperationPrecedence
public func .--> <U, V>(arg: U, transform: (U) -> V ) -> V {
	return transform(arg)
}

// MARK: - Logging stuff
public struct StderrOutputStream: TextOutputStream {
	public mutating func write(_ string: String) { fputs(string, stderr) }
}
public var errStream = StderrOutputStream()

var loggingEnabled = true
func log(_ message: String) {

	if loggingEnabled {
		print(message, to: &errStream)
	}
}
func fatal(_ message: String = "Fatal error!") -> Never  { log(message); abort() }

// Local Tests
if let inputFile = Bundle.main.path(forResource: "input", ofType: "txt") {
	freopen(inputFile, "r", stdin)
}

// MARK: - Cartesian 2d
typealias Int2d = (x: Int, y: Int)
func +(a: Int2d, b: Int2d) -> Int2d { return (a.x+b.x, a.y+b.y) }
func -(a: Int2d, b: Int2d) -> Int2d { return (a.x-b.x, a.y-b.y) }

// MARK: - Graph theory
typealias Edge = (u: Int, v: Int)

// MARK: - Random helpers

func xorshift128plus(seed0 : UInt64, seed1 : UInt64) -> () -> UInt64 {
	var s0 = seed0
	var s1 = seed1
	if s0 == 0 && s1 == 0 {
		s1 =  1 // The state must be seeded so that it is not everywhere zero.
	}

	return {
		var x = s0
		let y = s1
		s0 = y
		x ^= x << 23
		x ^= x >> 17
		x ^= y
		x ^= y >> 26
		s1 = x
		return s0 &+ s1
	}

}

struct Random {

	let generator = xorshift128plus(seed0: 0xDEAD_177EA7_15_1_1, seed1: 0x1234_0978_ABCD_CDAA)

	func bounded(to max: UInt64) -> UInt64 {
		var u: UInt64 = 0
		let b: UInt64 = (u &- max) % max
		repeat {
			u = generator()
		} while u < b
		return u % max
	}

	/// Random value for `Int` in arbitrary closed range, uniformally distributed
	subscript(range: CountableClosedRange<Int>) -> Int {
		let bound = range.upperBound.toIntMax() - range.lowerBound.toIntMax() + 1
		let x = range.lowerBound + Int(bounded(to: UInt64(bound)))

		guard range.contains(x) else { fatal("out of range") }
		return x
	}

	/// Random value for `Double` in arbitrary closed range
	subscript(range: ClosedRange<Double>) -> Double {
		let step = (range.upperBound - range.lowerBound) / Double(UInt64.max)

		let value = range.lowerBound + step*Double(generator())
		guard range.contains(value) else { fatal("out of range") }

		return value
	}

	/// Random value for `Double` in arbitrary half-open range
	subscript(range: Range<Double>) -> Double {
		let step = (range.upperBound - range.lowerBound) / (1.0 + Double(UInt64.max))

		let value = range.lowerBound + step*Double(generator())
		guard range.contains(value) else { fatal("out of range") }

		return value
	}

}

let random = Random()

/// MARK: - Array extension

extension Array  {

	var indexedDictionary: [Int: Element] {
		var result: [Int: Element] = [:]
		enumerated().forEach { result[$0.offset] = $0.element }
		return result
	}
}

extension Sequence {

	func group(_ comp: (Self.Iterator.Element, Self.Iterator.Element) -> Bool) -> [[Self.Iterator.Element]] {

		var result: [[Self.Iterator.Element]] = []
		var current: [Self.Iterator.Element] = []

		for element in self {
			if current.isEmpty || comp(element, current.last!) {
				current.append(element)
			} else {
				result.append(current)
				current = [element]
			}
		}

		if !current.isEmpty {
			result.append(current)
		}

		return result
	}
}

extension MutableCollection where Indices.Iterator.Element == Index {
	/// Shuffles the contents of this collection.
	mutating func shuffle() {
		let c = count
		guard c > 1 else { return }

		for (firstUnshuffled , unshuffledCount) in zip(indices, stride(from: c, to: 1, by: -1)) {
			let d: IndexDistance = random[0...numericCast(unshuffledCount-1)].-->numericCast
			guard d != 0 else { continue }
			let i = index(firstUnshuffled, offsetBy: d)
			swap(&self[firstUnshuffled], &self[i])
		}
	}
}

extension Sequence {
	/// Returns an array with the contents of this sequence, shuffled.
	func shuffled() -> [Iterator.Element] {
		var result = Array(self)
		result.shuffle()
		return result
	}
}


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// MARK: - ここテン
////////////////////////////////////////////////////////////////////////////////

enum EntityType: CustomStringConvertible {

	case factory
	case troop
	case bomb

	init?(_ string: String) {
		switch string {
			case "FACTORY": self = .factory
			case "TROOP": self = .troop
			case "BOMB": self = .bomb
			default: return nil
		}
	}

	var description: String {
		switch self {
			case .factory: return "FACTORY"
			case .troop: return "TROOP"
			case .bomb: return "BOMB"
		}
	}
}

struct Entity {

	var id: Int16
	var type: EntityType
	var arg1: Int16
	var arg2: Int16
	var arg3: Int16
	var arg4: Int16
	var arg5: Int16
}

extension Entity {

	init?(parseFrom line: String) {
		let input = line.components(separatedBy: " ").flatMap { String($0) }
		guard input.count == 7 else { return nil }

		guard let id = Int16(input[0]) else { return nil }
		guard let type = EntityType(input[1]) else { return nil }
		let args = input.suffix(from: 2).flatMap { Int16($0) }
		guard args.count == 5 else { return nil }

		self.init(
			id: id,
			type: type,
			arg1: args[0],
			arg2: args[1],
			arg3: args[2],
			arg4: args[3],
			arg5: args[4])
	}
}

extension Entity {

	/// Unique id incremented by one each time `createUnique` called
	static var uuid: Int16 = 1000

	static func createUnique(type: EntityType) -> Entity {

		let id = Entity.uuid; Entity.uuid += 1
		if Entity.uuid > 32000 { // overflow, reuse old uuids
			Entity.uuid = 1000
		}

		return Entity(id: id, type: type, arg1: 0, arg2: 0, arg3: 0, arg4: 0, arg5: 0)
	}
}

/// A kind of Entity, where type == EntityType.factory
struct Factory {

	/// Underlying plain struct `Entity`
	var entity: Entity

	/// Id
	var id: Int {
		get { return Int(entity.id) }
		set { entity.id = Int16(newValue) }
	}
	/// Type, must be EntityType.factory
	var type: EntityType {
		get { return entity.type }
		set { entity.type = newValue }
	}
	/// Player that owns the factory: 1 for you, -1 for your opponent and 0 if neutral
	var owner: Int {
		get { return Int(entity.arg1) }
		set { entity.arg1 = Int16(newValue) }
	}
	/// Number of cyborgs in the factory
	var units: Int {
		get { return Int(entity.arg2) }
		set { entity.arg2 = Int16(newValue) }
	}
	/// Factory production (between 0 and 3)
	var productionRate: Int {
		get { return Int(entity.arg3) }
		set { entity.arg3 = Int16(newValue) }
	}
	/// number of turns before the factory starts producing again 
	/// (0 means that the factory produces normally)
	var disabled: Int {
		get { return Int(entity.arg4) }
		set { entity.arg4 = Int16(newValue) }
	}

	static let ownerMe = 1
	static let ownerOpponent = -1
	static let ownerNeutral = 0

	init?(entity: Entity) {
		guard entity.type == .factory else { return nil }
		self.entity = entity
	}
}

/// A kind of Entity, where type == EntityType.troop
struct Troop {

	/// Underlying plain struct `Entity`
	var entity: Entity

	/// Id
	var id: Int {
		get { return Int(entity.id) }
		set { entity.id = Int16(newValue) }
	}
	/// Type, must be EntityType.troop
	var type: EntityType {
		get { return entity.type }
		set { entity.type = newValue }
	}
	/// Player that owns the troop: 1 for you or -1 for your opponent
	var owner: Int {
		get { return Int(entity.arg1) }
		set { entity.arg1 = Int16(newValue) }
	}
	/// Identifier of the factory from where the troop leaves
	var u: Int {
		get { return Int(entity.arg2) }
		set { entity.arg2 = Int16(newValue) }
	}
	/// Identifier of the factory targeted by the troop
	var v: Int {
		get { return Int(entity.arg3) }
		set { entity.arg3 = Int16(newValue) }
	}
	/// Number of cyborgs in the troop (positive integer)
	var unitCount: Int {
		get { return Int(entity.arg4) }
		set { entity.arg4 = Int16(newValue) }
	}
	/// remaining number of turns before the troop arrives (positive integer)
	var turnsLeft: Int {
		get { return Int(entity.arg5) }
		set { entity.arg5 = Int16(newValue) }
	}

	init?(entity: Entity) {
		guard entity.type == .troop else { return nil }
		self.entity = entity
	}
}

/// A kind of Entity, where type == EntityType.troop
struct Bomb {
	// TODO: bombs!
	var entity: Entity

	/// Id
	var id: Int {
		get { return Int(entity.id) }
		set { entity.id = Int16(newValue) }
	}
	/// Player that send the bomb: 1 if it is you, -1 if it is your opponent
	var owner: Int {
		get { return Int(entity.arg1) }
		set { entity.arg1 = Int16(newValue) }
	}
	/// Identifier of the factory from where the bomb is launched
	var source: Int {
		get { return Int(entity.arg2) }
		set { entity.arg2 = Int16(newValue) }
	}
	/// Identifier of the targeted factory if it's your bomb, -1 otherwise
	var destination: Int {
		get { return Int(entity.arg3) }
		set { entity.arg3 = Int16(newValue) }
	}
	/// Remaining number of turns before the bomb explodes
	/// (positive integer) if that's your bomb, -1 otherwise
	var turnsLeft: Int {
		get { return Int(entity.arg4) }
		set { entity.arg4 = Int16(newValue) }
	}

	init?(entity: Entity) {
		guard entity.type == .bomb else { return nil }
		self.entity = entity
	}
}

extension Troop: CustomStringConvertible
{
	var description: String {
		return "Troop<\(id)(\(owner))> (\(unitCount)@T-\(turnsLeft): \(u)->\(v))"
	}
}

struct World {

	typealias `Self` = World

	static var factoryCount = 0 // 7 ≤ factoryCount ≤ 15
	static var linkCount = 0 // 21 ≤ linkCount ≤ 105
	static var adjList: [[Int]] = []
	static var distance: [[Int]] = []

	var factories: [Factory] = []
	var troops: [Troop] = []
	var bombs: [Bomb] = []

	var turn = 0
}

enum Action {

	case wait
	case move(u: Int, v: Int, count: Int)
	case bomb(u: Int, v: Int)
	case inc(factory: Int)

	var description: String {
		switch self {
		case .wait:
			return "WAIT"
		case .move(let u, let v, let count):
			return "MOVE \(u) \(v) \(count)"
		case .bomb(let u, let v):
			return "BOMB \(u) \(v)"
		case .inc(let factory):
			return "INC \(factory)"
		}
	}

	static func printableArray(of elements: [Action]) -> String {
		return elements
			.map { $0.description }
			.joined(separator: "; ")
	}
}

extension Action {

	var executeOrder: Int {
		switch self {
			case .bomb: return 10
			case .move: return 20
			case .inc: return 30
			case .wait: return 100
		}
	}
}

struct PendingOrders {

	/// factory[i] will decrease amount of cyborgs by `value`
	var usedCyborgs: [Int] = [Int](repeating: 0, count: World.factoryCount)

	/// Move troops, use bombs, increase production
	var actions: [Action] = []
}

enum WorldSimulationOptions {
	case wait
	case increase
}


extension World {

	/// Game score. owner is 1 for me and -1 for opponent
	func score(owner: Int) -> Int {

		let inFactories = factories
			.filter { $0.owner == owner }
			.map { $0.units }
			.reduce(0, +)
		let inTroops = troops
			.filter { $0.owner == owner }
			.map { $0.unitCount }
			.reduce(0, +)

		return inFactories + inTroops
	}

	/// Game score. owner is 1 for me and -1 for opponent
	func scoreProduction(owner: Int) -> Int {

		return factories
			.filter { $0.owner == owner }
			.map { factory in 10 * factory.productionRate }
			.reduce(0, +)
	}

	/// Score production as 10 points
	func scoreDifference(includeProduction: Bool) -> Int {

		var scoreMe = score(owner: Factory.ownerMe)
		var scoreOpponent = score(owner: Factory.ownerOpponent)

		if includeProduction {
			scoreMe += scoreProduction(owner: Factory.ownerMe)
			scoreOpponent += scoreProduction(owner: Factory.ownerOpponent)
		}

		return scoreMe - scoreOpponent
	}

	func scoreDiff(factory fid: Int, turns: Int, delayed: Int, orders: PendingOrders, newAction: Action) -> Int {
		guard delayed <= 1 else { log("Not expeced long delays"); return -999 }
		guard case let .move(newMove) = newAction else { return -999 }

		var score = 0
		var factory = factories[fid]
		var filteredTroops = troops
			.filter { troop in troop.v == fid }
		var filteredBombs = bombs
			.filter { bomb in bomb.destination == fid }
		var delta = 1
		var evaluatedTurn = 0
		var newOrders = orders
		while evaluatedTurn < turns {
			// ---
			// Move troops
			// ---
			move(troops: &filteredTroops, turns: delta)
			move(bombs: &filteredBombs, turns: delta)

			// ---
			// Decrease disabled countdown
			// ---
			factory.disabled = max(0, factory.disabled - delta)

			// ---
			// Execute orders
			// ---
			do {
				defer {	newOrders.actions.removeAll() }

				if delayed == evaluatedTurn {
					newOrders.actions.append(newAction)
					newOrders.usedCyborgs[newMove.u] += newMove.count
				}
				for action in newOrders.actions {
					switch action {
						case let .bomb(bombAction):
							let baseEntity = Entity.createUnique(type: .bomb)
							guard var bomb = Bomb(entity: baseEntity) else { log("Cannot create bomb with \(action)"); continue }

							bomb.owner = Factory.ownerMe
							bomb.source = bombAction.u
							bomb.destination = bombAction.v
							bomb.turnsLeft = World.distance[bombAction.u][bombAction.v]

							if bombAction.v == fid {
								filteredBombs.append(bomb)
							}

						case let .inc(id):
							if id == fid && factory.productionRate < 3 && factory.units >= 10 {
								factory.productionRate += 1
								factory.units -= 10
							}

						case let .move(moveAction):
							// TODO: I assume check is done before calling this method, but are they?
							let baseEntity = Entity.createUnique(type: .troop)
							guard var troop = Troop(entity: baseEntity) else { log("Cannot create troop with \(action)"); continue}

							troop.owner = Factory.ownerMe
							troop.u = moveAction.u
							troop.v = moveAction.v
							troop.unitCount = moveAction.count
							troop.turnsLeft = World.distance[moveAction.u][moveAction.v]
							if moveAction.u == fid {
								factory.units -= moveAction.count
							}
							if moveAction.v == fid {
								filteredTroops.append(troop)
							}

						case .wait:
							break
					}
				}
			}

			// ---
			// Create new units
			// ---
			if factory.owner != Factory.ownerNeutral {
				factory.units += factory.productionRate * delta
			}

			// ---
			// Solve Battles
			// ---
			do {
				let readyForBattle = filteredTroops.filter { troop in troop.turnsLeft <= 0 }
				defer {
					filteredTroops = filteredTroops.filter { troop in troop.turnsLeft > 0 }
				}

				var forces: BattleForces = (me: 0, you: 0)
				for troop in readyForBattle {
					if troop.owner == Factory.ownerMe {
						forces.me += troop.unitCount
					}
					else {
						forces.you += troop.unitCount
					}
				}

				solveBattle(factory: &factory, forces: forces)
			}

			// ---
			// Solve bombs
			// ---
			do {
				let readyToExplode = filteredBombs.filter { bomb in bomb.turnsLeft <= 0 }
				defer {
					filteredBombs = filteredBombs.filter { bomb in bomb.turnsLeft > 0 }
				}

				for bomb in readyToExplode {
					guard bomb.destination == fid else { continue }

					let damage = min(factory.units, max(10, factory.units/2))
					factory.units -= damage
					factory.disabled = 5
				}
			}

			// ---
			// Inc if you can!
			// ---
//			if factory.owner != Factory.ownerNeutral && evaluatedTurn < turns - 11 {
//				if filteredTroops.filter({ troop in troop.owner != factory.owner }).count == 0 {
//					// No troops can stop us!
//					// Increase  production
//					if factory.productionRate < 3 && factory.units >= 10 {
//						let newAction = Action.inc(factory: factory.id)
//						newOrders.actions.append(newAction)
//					}
//				}
//			}

			// ---
			// Calculate time to nearest event
			// ---
			do {
				evaluatedTurn += delta
				if delayed == evaluatedTurn {
					delta = 1
					continue
				}
				if newOrders.actions.count > 0 {
					delta = 1
					continue
				}
				let max = turns - evaluatedTurn
				let disabledEnds = factory.disabled > 0
					? factory.disabled
					: max
				let troopEvent = filteredTroops
					.reduce(max) { acc, troop in
						return min(acc, troop.turnsLeft)
					}
				let bombEvent = filteredBombs
					.reduce(max) { acc, bomb in
						return min(acc, bomb.turnsLeft)
					}

				delta = [max, disabledEnds, troopEvent, bombEvent].min()!
				if delta == 0 && evaluatedTurn < turns {
					log("Have unprecessed events on loop end.")
					return -999
				}
			}
		}

		score += factory.owner * factory.units
		score += 1 * min(3 - factory.productionRate, factory.units / 10) // Inc potencial is bonus
		return score
	}

	func move(bombs: inout [Bomb], turns: Int = 1) {

		for i in bombs.indices {
			bombs[i].turnsLeft -= turns
		}
	}

	func move(troops: inout [Troop], turns: Int = 1) {

		for i in troops.indices {
			troops[i].turnsLeft -= turns
		}
	}

	typealias BattleForces = (me: Int, you: Int)
	func solveBattle(factory: inout Factory, forces: BattleForces)
	{
		// Troops fight with each other first
		let kia = min(forces.me, forces.you)

		var owner: Int
		var remainingUnits: Int
		if forces.me > kia {
			owner = Factory.ownerMe
			remainingUnits = forces.me - kia
		}
		else if forces.you > kia {
			owner = Factory.ownerOpponent
			remainingUnits = forces.you - kia
		}
		else {
			return
		}

		// Remaining troops fight with factory defences
		if factory.owner == owner { // same owner
			factory.units += remainingUnits
		}
		else { // fight with units defenging factory
			if remainingUnits > factory.units { // Attacking forces win, change owner
				factory.owner = owner
				factory.units = remainingUnits - factory.units
			}
			else { // Defences win
				factory.units -= remainingUnits
			}
		}
	}

	/// World state on next turn assuming WAIT action from players
	func nextTurn(with orders: PendingOrders? = nil, options: WorldSimulationOptions) -> World {

		// ---
		// Move troops and bombs
		// ---
		var newTroops: [Troop] = []
		var battles: [[Troop]] = []
		for troop in troops.sorted(by: { a, b in a.v < b.v }) {
			var newTroop = troop
			newTroop.turnsLeft = troop.turnsLeft - 1

			if newTroop.turnsLeft > 0 {
				newTroops.append(newTroop)
			}
			else {
				if battles.isEmpty {
					battles.append([newTroop])
				}
				else {
					if newTroop.v == battles.last!.last!.v {
						var last = battles.popLast()!
						last.append(newTroop)

						battles.append(last)
					}
					else {
						battles.append([newTroop])
					}
				}
			}
		}
		var newBombs: [Bomb] = []
		var bombsReadyToExplode: [Bomb] = []
		for bomb in bombs {
			var newBomb = bomb
			newBomb.turnsLeft = bomb.turnsLeft - 1

			log("bomb has \(bomb.turnsLeft) turns left")
			if newBomb.turnsLeft > 0 {
				newBombs.append(newBomb)
			}
			else {
				bombsReadyToExplode.append(newBomb)
			}
		}

		// ---
		// Decrease disabled countdown
		// ---
		var newFactories: [Factory] = []
		for factory in factories {
			var newFactory = factory
			newFactory.disabled = factory.disabled > 0
				? factory.disabled - 1
				: 0

			newFactories.append(newFactory)
		}

		// ---
		// Execute orders
		// TODO: owner is only me?
		// ---
		if let orders = orders {
			// Send bombs
			let actions = orders.actions.sorted { $0.executeOrder < $1.executeOrder }
			var veryNewTroops: [Troop] = []
			var veryNewBombs: [Bomb] = []

			for action in actions {
				switch action {
					case .bomb(let u, let v):
						if veryNewBombs.first(where: { bomb in bomb.source == u && bomb.destination == v }) == nil {
							/// Skip bombs with same source and destination
							let baseEntity = Entity.createUnique(type: .bomb)
							guard var bomb = Bomb(entity: baseEntity) else { log("Cannot create bomb with \(action)"); continue }

							bomb.owner = Factory.ownerMe
							bomb.source = u
							bomb.destination = v
							bomb.turnsLeft = World.distance[u][v]
							veryNewBombs.append(bomb)
						} else {
							log("Cannot add bomb(already have a bomb here) with \(action)")
						}

					case .move(let u, let v, let count):
						let unitsToMove = min(count, newFactories[u].units)
						guard unitsToMove > 0 else {
							log("Nothing to move with \(action)")
							continue
						}
						guard nil == veryNewBombs.first(where: { bomb in bomb.source == u && bomb.destination == v }) else {
							log("Will not send troops (there is also a bomb) when parsing \(action)")
							continue
						}
						if let i = veryNewTroops.index(where: { troop in troop.u == u && troop.v == v }) {
							log("Troop already exists! adding to it  with \(action)")
							veryNewTroops[i].unitCount += unitsToMove
							newFactories[u].units -= unitsToMove
						}
						else {
							let baseEntity = Entity.createUnique(type: .troop)
							guard var troop = Troop(entity: baseEntity) else { log("Cannot create troop with \(action)"); continue}
							troop.owner = Factory.ownerMe
							troop.u = u
							troop.v = v
							troop.unitCount = unitsToMove
							troop.turnsLeft = World.distance[u][v]
							newFactories[u].units -= unitsToMove

							veryNewTroops.append(troop)
						}

					case .inc(let factory):
						if newFactories[factory].productionRate < 3 && newFactories[factory].units >= 10 {
							newFactories[factory].productionRate += 1
							newFactories[factory].units -= 10
						}
						else {
							log("cannot upgrade factory \(factory)")
						}

					case .wait:
						continue
				}
			}

			newBombs.append(contentsOf: veryNewBombs)
			newTroops.append(contentsOf: veryNewTroops)
		}

		// ---
		// Give orders based on default begavior in options
		// ---
		switch options {
			case .increase:
				for factory in newFactories {
					guard factory.owner != Factory.ownerNeutral else { continue }
					while newFactories[factory.id].productionRate < 3 && newFactories[factory.id].units >= 10 {
						newFactories[factory.id].productionRate += 1
						newFactories[factory.id].units -= 10
					}
				}

			case .wait:
				break
		}


		// ---
		// Create new units
		// ---
		for (i, factory) in newFactories.enumerated() {
			guard factory.owner != Factory.ownerNeutral else { continue }
			guard factory.disabled <= 0 else { continue }

			newFactories[i].units += factory.productionRate
		}

		// ---
		// Solve battles
		// ---
		for battle in battles {

			let i = battle.first!.v

			let forces: BattleForces = battle
				.reduce((me: 0, you: 0)) { acc, troop in
					return (
						me: troop.owner == Factory.ownerMe
							? acc.me + troop.unitCount
							: acc.me,
						you: troop.owner == Factory.ownerOpponent
							? acc.you + troop.unitCount
							: acc.you)
				}

			solveBattle(factory: &newFactories[i], forces: forces)
		}

		// ---
		// Solve bombs
		// ---
		for bomb in bombsReadyToExplode {
			let factory = newFactories[bomb.destination]
			let damage = min(factory.units, max(10, factory.units/2))
			newFactories[bomb.destination].units = factory.units - damage
			newFactories[bomb.destination].disabled = 5
		}

		return World(
			factories: newFactories,
			troops: newTroops,
			bombs: newBombs,
			turn: turn+1)
	}
}

struct BombAdviser {

	struct LaunchRecord {
		var bomb: Bomb
		var resolvedBomb: Bomb
		let world: World
	}

	var registeredLaunches: [Int:LaunchRecord] = [:]

	mutating func guessTarget(for bomb: Bomb, in world: World) -> Bomb {

		var bomb = updateLaunchResolvedBomb(for: bomb, in: world)
		let turnsFromLaunch = world.turn - registeredLaunches[bomb.id]!.world.turn
		bomb.turnsLeft -= turnsFromLaunch

		return bomb
	}

	/// Try to guess hidden parameters
	mutating func updateLaunchResolvedBomb(for bomb: Bomb, in world: World) -> Bomb {

		if let launch = registeredLaunches[bomb.id] {
			let turnsFromLaunch = world.turn - launch.world.turn
			if turnsFromLaunch >= launch.resolvedBomb.turnsLeft {
				log("Wrong expectation for bomb: \(bomb)")
				let record = resolve(bomb: launch.bomb, world: launch.world, minDuration: turnsFromLaunch + 1)

				registeredLaunches[record.resolvedBomb.id] = record
				return record.resolvedBomb
			}
			else {
				return launch.resolvedBomb
			}
		}
		else {
			let record = resolve(bomb: bomb, world: world, minDuration: 1)

			registeredLaunches[bomb.id] = record
			return record.resolvedBomb
		}
	}

	func resolve(bomb: Bomb, world: World, minDuration: Int) -> LaunchRecord {

		let target = world.factories
			.filter { target in
				target.id != bomb.source
			}
			.filter { target in
				World.distance[bomb.source][target.id] >= minDuration
			}
			.sorted{ a, b in
				let distA = World.distance[bomb.source][a.id]
				let distB = World.distance[bomb.source][b.id]

				// In search prefer:
				// 1. owner is me
				// 2. more production rate
				// 3. more units
				// 4. less distance
				if a.owner != b.owner {
					return a.owner > b.owner
				}
				else if a.productionRate != b.productionRate {
					return a.productionRate > b.productionRate
				}
				else if a.units != b.units {
					return a.units > b.units
				}
				else {
					return distA < distB
				}
			}
			.first

		if let target = target {
			var resolved = bomb
			resolved.destination = target.id
			resolved.turnsLeft = World.distance[bomb.source][target.id]

			return LaunchRecord(bomb: bomb, resolvedBomb: resolved, world: world)
		}
		else {
			log("!!! Cannot find suitable target for bomb=\(bomb)! ")
			var resolved = bomb
			resolved.destination = 0
			resolved.turnsLeft = minDuration

			return LaunchRecord(bomb: bomb, resolvedBomb: resolved, world: world)
		}
	}
}

var bombAdviser = BombAdviser()


class StrategyAlgorithmHelper {

	/// world
	let world: World
	init(world: World) {
		self.world = world
	}

	/// Factories with owner == ownerMe
	lazy var ownedFactories: [Int] = { return self.getOwnedFactories() }()
	private func getOwnedFactories() -> [Int] {

		var owned: [Int] = []
		for factory in world.factories {
			if factory.owner == Factory.ownerMe {
				owned.append(factory.id)
			}
		}

		return owned
	}

	/// Factories with neutral owner, but reachable from owned
	lazy var unownedReachable: Set<Int> = { return self.getUnownedReachable() }()
	private func getUnownedReachable() -> Set<Int> {

		var reachable: Set<Int> = []
		for u in self.ownedFactories {
			for v in World.adjList[u] {
				let factory = world.factories[v]
				if factory.owner == Factory.ownerNeutral {
					reachable.insert(v)
				}
			}
		}

		return reachable
	}

	typealias FactoryWithDistance = (factory: Factory, dist: Int)
	func closestFactories(to u: Int, owner: Int) -> [FactoryWithDistance] {
		var unsorted: [FactoryWithDistance] = []

		for v in 0..<World.factoryCount {
			let target = world.factories[v]
			guard target.owner == owner else { continue }

			let dist = World.distance[u][v]
			if dist < 999 {
				unsorted.append((factory: target, dist: dist))
			}
		}

		return unsorted
			.sorted{ (lhs, rhs) -> Bool in
				return lhs.dist == rhs.dist
					? lhs.factory.id < rhs.factory.id
					: lhs.dist < rhs.dist
			}
	}

	/// Factories with owner == ownerMe, sorted by id
	lazy var myFactories: [Factory] = { return self.getMyFactories() }()
	private func getMyFactories() -> [Factory] {

		return world.factories
			.filter { factory in
				factory.owner == Factory.ownerMe
			}
			.sorted { a, b in
				a.id < b.id
			}
	}

	lazy var dangerFactor: [Int] = { return self.getDangerFactor() }()
	private func getDangerFactor() -> [Int] {

		var targets: [Int] = []
		for v in 0..<world.factories.count {
			var danger = 0
			for u in 0..<world.factories.count {
				guard u != v else { continue }
				guard world.factories[u].owner == Factory.ownerOpponent else { continue }

				danger += (500+10*world.factories[u].units)/World.distance[u][v]
			}
			targets.append(danger)
		}

		return targets
	}

	typealias FactoryEdgeEx = (from: Factory, to: Factory, dist: Int)
	/// Sorting order by less distacne, source id, target id
	lazy var myEdgesEx: [FactoryEdgeEx] = { return self.getMyExtendedEdges() }()
	private func getMyExtendedEdges() -> [FactoryEdgeEx] {

		var extendedEdges: [FactoryEdgeEx] = []

		for factory in myFactories {
			for v in 0..<World.factoryCount {
				let dist = World.distance[factory.id][v]
				guard dist < 999 else { continue }

				extendedEdges.append((from: factory, to: world.factories[v], dist: dist))
			}
		}

		// sort edges
		extendedEdges
			.sort { a, b in
				if dangerFactor[a.to.id] != dangerFactor[b.to.id] {
					return dangerFactor[a.to.id] < dangerFactor[b.to.id]
				}
				else if a.to.owner != Factory.ownerMe || b.to.owner != Factory.ownerMe {
					if a.to.owner == Factory.ownerMe || b.to.owner == Factory.ownerMe {
						return a.to.owner < b.to.owner
					}

					let Adist = a.dist - a.to.owner*a.to.productionRate
					let Bdist = b.dist - b.to.owner*b.to.productionRate
					if Adist != Bdist {
						return Adist < Bdist
					}
					else if a.from.id != b.from.id {
						return a.from.id < b.from.id
					}
					else {
						return a.to.id < b.to.id
					}
				}
				else {
					if a.dist != b.dist {
						return a.dist < b.dist
					}
					else if a.from.id != b.from.id {
						return a.from.id < b.from.id
					}
					else {
						return a.to.id < b.to.id
					}
				}
			}

		return extendedEdges
	}

	/// Binary search for maximum of one-dimension function `calc` on closed interval of Int arguments
	func binarySearch(_ closedRange: CountableClosedRange<Int>, maxTap: Int = 5, calc: (Int) -> Int) -> Int {
		guard closedRange.count > 0 else { return 0 }

		var l = closedRange.lowerBound, r = closedRange.upperBound
		var valueL = calc(l)
		var valueR = calc(r)

		var mid = l
		var valueM = valueL
		var tap = 2
		while l + 1 < r && tap < maxTap {
			mid = (l + r) / 2
			valueM = calc(mid)
			tap += 1

			if valueL >= valueR {
				r = mid
				valueR = valueM
			}
			else {
				l = mid
				valueL = valueM
			}
		}

		let valueMax = [valueL, valueM, valueR].max()!
		if valueL == valueMax {
			return l
		}
		else if valueM == valueMax {
			return mid
		}
		else {
			return r
		}
	}

	/// Helper constants for golden ratio
	enum PhiSlice: Double {

		case left = 0.3819660115 // 1.0 - 0.5*(sqrt(5.0) - 1)
		case right = 0.6180339885 // 0.5*(sqrt(5.0) - 1)

		var flipped: PhiSlice {

			switch self {
				case .left: return .right
				case .right: return .left
			}
		}
	}

	/// Select a golden ratio point
	func slicePoint(_ spin: PhiSlice, l: Int16, r: Int16) -> Int16 {
		guard r - l > 1 else { log("Wrong slice"); return r }

		let slicable = r - l - 2
		return l + 1 + Int(round(Double(slicable) * spin.rawValue))
	}

	typealias CaseAlphaResult = (Int16, Int)
	/// Golden ratio search subcase for searching in range (l;r]
	func caseAlpha(l: Int16, r: Int16, valueR: Int, spin: PhiSlice, maxDepth: Int16, calc: (Int) -> Int) -> CaseAlphaResult {
		guard r - l > 1 else { return (r, valueR) }
		guard maxDepth > 0 else { return (r, valueR) }

		let mid = slicePoint(spin, l: l, r: r)
		let valueM = calc(Int(mid))

		if valueM >= valueR {
			/// continue search in (l; mid]
			return caseAlpha(l: l, r: mid, valueR: valueM, spin: .left, maxDepth: maxDepth-1, calc: calc)
		}
		else {
			/// continue search in (mid; R]
			return caseAlpha(l: mid, r: r, valueR: valueR, spin: .right, maxDepth: maxDepth-1, calc: calc)
		}
	}

	/// Binary search for maximum of one-dimension function `calc` on closed interval of Int arguments
	/// Use CaseAlphaResult to reduce calc
	func goldenSearch(_ closedRange: CountableClosedRange<Int>, maxTap: Int = 6, calc: (Int) -> Int) -> Int {
		guard closedRange.count > 0 else { return 0 }

		let l = closedRange.lowerBound, r = closedRange.upperBound
		let valueR = calc(r)

		let result = caseAlpha(l: Int16(l), r: Int16(r), valueR: valueR, spin: .left, maxDepth: Int16(maxTap-2), calc: calc)

		let valueL = calc(l)
		if valueL >= result.1 {
			return l
		}
		else {
			return Int(result.0)
		}
	}

	func fastScore(newAction: Action, orders: PendingOrders, turns: Int, delayed: Int) -> Int {
		guard case let .move(moveAction) = newAction else { log("Only .move expected"); return -999 }

		let scoreU = world.scoreDiff(factory: moveAction.u, turns: turns, delayed: delayed, orders: orders, newAction: newAction)
		let scoreV = world.scoreDiff(factory: moveAction.v, turns: turns, delayed: delayed, orders: orders, newAction: newAction)
		return scoreU + scoreV
	}

	func score(after turns: Int, orders: PendingOrders, newAction: Action, options: WorldSimulationOptions) -> Int {

		guard turns > 11 else { log("Cannot score for \(turns) turns."); return -999 }

		let loggingWas = loggingEnabled
		loggingEnabled = false
		defer { loggingEnabled = loggingWas }

		var orderCopy = orders

		switch newAction {
			case .move(let u, _, let units):
				if units > 0  {
					orderCopy.actions.append(newAction)
					orderCopy.usedCyborgs[u] += units
				}

			case .inc(let factory):
				if world.factories[factory].owner == Factory.ownerMe
					&& world.factories[factory].units - orderCopy.usedCyborgs[factory] >= 10 {

					orderCopy.actions.append(newAction)
					orderCopy.usedCyborgs[factory] += 10
				}

			case .wait:
				break

			default:
				log("Unexpected action \(newAction)")
				return 0
		}


		var evolvingWorld = world.nextTurn(with: orderCopy, options: .wait)
		for _ in 1...9 {
			evolvingWorld = evolvingWorld.nextTurn(options: .wait)
		}
		for _ in 10...20 {
			evolvingWorld = evolvingWorld.nextTurn(options: options)
		}

		return evolvingWorld.scoreDifference(includeProduction: true)
	}

	func delayedScore(after turns: Int, orders: PendingOrders, newAction: Action, options: WorldSimulationOptions) -> Int {

		guard turns > 11 else { log("Cannot score for \(turns) turns."); return -999 }

		let loggingWas = loggingEnabled
		loggingEnabled = false
		defer { loggingEnabled = loggingWas }

		var evolvingWorld = world.nextTurn(with: orders, options: .wait)
		var newOrder = PendingOrders()

		switch newAction {
			case .move(let u, _, let units):
				let remainedUnits = min(units, evolvingWorld.factories[u].units)
				if( remainedUnits > 0 && evolvingWorld.factories[u].owner == Factory.ownerMe) {
					newOrder.actions.append(newAction)
					newOrder.usedCyborgs[u] = remainedUnits
				}

			case .inc(let factory):
				if evolvingWorld.factories[factory].owner == Factory.ownerMe
					&& evolvingWorld.factories[factory].units >= 10 {

					newOrder.actions.append(newAction)
					newOrder.usedCyborgs[factory] += 10
				}

			case .wait:
				break

			default:
				log("Unexpected action \(newAction)")
				return 0
		}


		evolvingWorld = evolvingWorld.nextTurn(with: newOrder, options: .wait)
		for _ in 2...9 {
			evolvingWorld = evolvingWorld.nextTurn(options: .wait)
		}
		for _ in 10...20 {
			evolvingWorld = evolvingWorld.nextTurn(options: options)
		}

		return evolvingWorld.scoreDifference(includeProduction: true)
	}
}

protocol ChainableStrategyProtocol {

	var name: String { get }
	var input: PendingOrders { get set }
	var output: PendingOrders { get }
}

struct WaitStrategy: ChainableStrategyProtocol {

	var input = PendingOrders()
	var output: PendingOrders {
		var output = input
		output.actions.append(.wait)

		return output
	}
	var name: String = "WaitStrategy"
}

struct BombStrategy: ChainableStrategyProtocol {

	static var bombsLeft = 2

	/// Algorithm helper
	let helper: StrategyAlgorithmHelper

	/// Filter targets that has not enough production
	var minProduction: Int

	/// Filter targets that have not enough units
	var minUnits: Int

	/// Filter targets that are not close enough
	var maxDistance: Int

	var name: String {
		return "BombStrategy p>=\(minProduction) u>=\(minUnits) d<=\(maxDistance)"
	}

	init(helper: StrategyAlgorithmHelper, minProduction: Int = 0, minUnits: Int = 0, maxDistance: Int = 99, input: PendingOrders = PendingOrders()) {
		self.helper = helper
		self.minProduction = minProduction
		self.minUnits = minUnits
		self.maxDistance = maxDistance
		self.input = input
	}

	var input: PendingOrders

	var output: PendingOrders {
		var output = input

		// TODO: add bombs
		for edgeEx in helper.myEdgesEx.sorted(by: {a, b in a.dist < b.dist}) {
			guard edgeEx.to.owner == Factory.ownerOpponent else { continue }
			guard edgeEx.to.productionRate >= minProduction else { continue }
			guard edgeEx.to.units >= minUnits else { continue }
			guard edgeEx.dist <= maxDistance else { continue }

			let u = edgeEx.from.id
			let v = edgeEx.to.id

			let samePathAction = output.actions.first { action in
				switch action {
					case .bomb(_, let bv):
						return v == bv

					case .move(let mu, let mv, _):
						return mu == u && mv == v

					default:
						return false

				}
			}
			guard samePathAction == nil else { continue }

			let sameDestination = helper.world.bombs
				.first{ bomb in
					bomb.destination == v
				}
			guard sameDestination == nil else { continue }

			output.actions.append(.bomb(u: edgeEx.from.id, v: edgeEx.to.id))
		}

		return output
	}
}

struct IncSafe: ChainableStrategyProtocol {

	/// Algorithm helper
	let helper: StrategyAlgorithmHelper

	init(helper: StrategyAlgorithmHelper, input: PendingOrders = PendingOrders()) {
		self.helper = helper
		self.input = input
	}

	var name: String { return "IncSafe" }

	var input: PendingOrders

	var output: PendingOrders {

		var output = input

		for factory in helper.myFactories {
			let fid = factory.id
			var unitsAvailable = factory.units - input.usedCyborgs[fid]
			var productionRate = factory.productionRate

			let ETA = helper.world.bombs
				.filter { bomb in bomb.destination == fid}
				.reduce(99) { acc, bomb in min(acc, bomb.turnsLeft) }
			if ETA <= 15 { continue }

			while unitsAvailable >= 10 && productionRate < 3 {

				let delayedScore = helper.delayedScore(after: 20, orders: output, newAction: .inc(factory: fid), options: .wait)
				let immediateScore = helper.score(after: 20, orders: output, newAction: .inc(factory: fid), options: .wait)

				if immediateScore > delayedScore {
					output.actions.append(.inc(factory: fid))
					unitsAvailable -= 10
					output.usedCyborgs[fid] += 10
					productionRate += 1
				}
				else {
					break
				}
			}
		}

		return output
	}
}


struct WatchDog {

	var begin = clock()

	var greenZone = 10
	var yellowZone = 30
	var redZone = 42

	var isBellowGreen: Bool {
		return self.millisecondsFromBegin() < greenZone
	}

	var isBellowYellow: Bool {
		return self.millisecondsFromBegin() < yellowZone
	}

	var isBellowRed: Bool {
		return self.millisecondsFromBegin() < redZone
	}

	mutating func adjustZones(maxTime: Int) {

		self.greenZone = maxTime * 10 / 50
		self.yellowZone = maxTime * 30 / 50
		self.redZone = maxTime * 42 / 50
	}

	mutating func reset() {

		self.begin = clock()
	}

	func millisecondsFromBegin() -> Int {

		let elapsed = Double(clock() - begin) / Double(CLOCKS_PER_SEC)
		return Int(elapsed * 1_000)
	}
}

var watchDog = WatchDog()

struct SmarterMovement: ChainableStrategyProtocol {

	/// Algorithm helper
	let helper: StrategyAlgorithmHelper

	init(helper: StrategyAlgorithmHelper, input: PendingOrders = PendingOrders(), shouldShuffle: Bool) {
		self.helper = helper
		self.input = input
		self.shouldShuffle = shouldShuffle
	}

	var name: String { return "SmarterMovement" }

	var input: PendingOrders

	let shouldShuffle: Bool

	var output: PendingOrders {

		var output = input

		let total = helper.myEdgesEx.count
		var processed = 0

		let edges = shouldShuffle
			? helper.myEdgesEx.shuffled()
			: helper.myEdgesEx
		for edgeEx in edges {
			guard watchDog.isBellowRed else { break }
			processed += 1

			let u = edgeEx.from.id
			let v = edgeEx.to.id

			let availableUnits = helper.world.factories[u].units - output.usedCyborgs[u]
			let best = helper
				.goldenSearch(0...availableUnits, maxTap: 541 / (total+1)) { units in
					let score = helper.fastScore(newAction: Action.move(u: u, v: v, count: units), orders: output, turns: 20, delayed: 0)
					return score
				}

			if best > 0 {
				let delayedScore = helper.fastScore(newAction: Action.move(u: u, v: v, count: best), orders: output, turns: 20, delayed: 1)
				let immediateScore = helper.fastScore(newAction: Action.move(u: u, v: v, count: best), orders: output, turns: 20, delayed: 0)

				if immediateScore > delayedScore {
					output.actions.append(.move(u: u, v: v, count: best))
					output.usedCyborgs[u] += best
				}
			}
		}

		log("Processed \(processed)/\(total) edges");
		return output
	}
}

struct PushForces: ChainableStrategyProtocol {

	/// Algorithm helper
	let helper: StrategyAlgorithmHelper

	init(helper: StrategyAlgorithmHelper, input: PendingOrders = PendingOrders()) {
		self.helper = helper
		self.input = input
	}

	var name: String { return "PushForces" }

	var input: PendingOrders

	var output: PendingOrders {

		var ordersCopy = input

		var goodScore = helper.score(after: 20, orders: ordersCopy, newAction: .wait, options: .wait)

		let lessDangerFirst = helper.myFactories.sorted(by: { a, b in helper.dangerFactor[a.id] < helper.dangerFactor[b.id]})
		for u in lessDangerFirst {
			for v in helper.myFactories.sorted(by: { a, b in World.distance[u.id][a.id] < World.distance[u.id][b.id]}) {
				if helper.dangerFactor[v.id] > helper.dangerFactor[u.id] {
					let available = u.units - ordersCopy.usedCyborgs[u.id]
					guard available > 1  else { continue }

					let newAction = Action.move(u: u.id, v: v.id, count: available / 2)
					let newScore = helper.score(after: 20, orders: ordersCopy, newAction: newAction, options: .wait)
					if newScore >= goodScore {
						ordersCopy.actions.append(newAction)
						ordersCopy.usedCyborgs[u.id] += available / 2
						goodScore = newScore
					}
				}
			}

			guard watchDog.isBellowRed else { break }
		}

		return ordersCopy
	}
}


struct StrategyFactory {

	/// Algorithm Helper
	let algorithmHelper: StrategyAlgorithmHelper

	typealias ChainedStrategyOrders = (strategyNames: [String], orders: PendingOrders)
	func makeChain() -> ChainedStrategyOrders {

		var usedStrategies: [String] = []

		var orders = PendingOrders()
		if BombStrategy.bombsLeft > 0 {
			let bombStrategy: BombStrategy
			switch algorithmHelper.world.turn {
				case 0..<10:
					bombStrategy = BombStrategy(helper: algorithmHelper, minProduction: 3, minUnits: 5, maxDistance: algorithmHelper.world.turn+1, input: orders)

				case 10...50:
					bombStrategy = BombStrategy(helper: algorithmHelper, minProduction: 3, minUnits: 10, maxDistance: 12, input: orders)

				case 50..<100:
					bombStrategy = BombStrategy(helper: algorithmHelper, minProduction: 2, minUnits: 15, maxDistance: 15, input: orders)

				case 100..<150:
					bombStrategy = BombStrategy(helper: algorithmHelper, minProduction: 3, minUnits: 10, maxDistance: 20, input: orders)

				default:
					bombStrategy = BombStrategy(helper: algorithmHelper, minProduction: 1, minUnits: 0, maxDistance: 50, input: orders)
			}
			orders = bombStrategy.output

			let bombsUsed = orders.actions
				.filter { action in
					switch action {
						case .bomb: return true
						default: return false
					}
				}
				.count

			if bombsUsed > 0 {
				BombStrategy.bombsLeft -= bombsUsed
				usedStrategies.append(bombStrategy.name)
			}

		}


		// Smart
		var bestScore = -9999
		var bestOrders = orders
		for shuffles in 0..<12  {
			guard watchDog.isBellowRed else { break }

			var ordersCopy = orders

			var incSafeCalculated = false
			if world.turn > 15 || bombAdviser.registeredLaunches.count > 0 {
				let incSafe = IncSafe(helper: algorithmHelper, input: ordersCopy)
				ordersCopy = incSafe.output
				incSafeCalculated = true
			}

			let shouldShuffle = shuffles > 0
			let smart = SmarterMovement(helper: algorithmHelper, input: ordersCopy, shouldShuffle: shouldShuffle)
			ordersCopy = smart.output

			if !incSafeCalculated {
				let incSafe = IncSafe(helper: algorithmHelper, input: ordersCopy)
				ordersCopy = incSafe.output
			}

			let pushForces = PushForces(helper: algorithmHelper, input: ordersCopy)
			ordersCopy = pushForces.output

			let newScore = algorithmHelper.score(after: 20, orders: ordersCopy, newAction: .wait, options: .increase)
			if newScore > bestScore {
				bestScore = newScore
				bestOrders = ordersCopy
			}
		}
		orders = bestOrders

		return (strategyNames: usedStrategies, orders: orders)
	}
}


////////////////////////////////////////////////////////////////////////////////
// MARK: - Main / preinitialization loop
////////////////////////////////////////////////////////////////////////////////

var world = World()

var turn = 0
// turn 0

func rl() -> String? {
	if let line = readLine() {
//		log(line)
		return line
	}
	else {
		return nil
	}
}

World.factoryCount = Int(rl()!)! // 7 ≤ factoryCount ≤ 15
world.factories = [Factory](repeating: Factory(entity: Entity.createUnique(type: .factory))!, count: World.factoryCount)
World.linkCount = Int(rl()!)! // 21 ≤ linkCount ≤ 105
World.adjList = [[Int]](repeating: [], count: World.factoryCount)
World.distance = repeatElement(repeatElement(Int.max/2, count: World.factoryCount).map{$0}, count: World.factoryCount).map{$0}

for _ in 0..<World.linkCount {
	let arr = rl()!.components(separatedBy: " ").flatMap { Int($0) }
	guard arr.count == 3 else { fatal("arr=\(arr)") }

	World.adjList[arr[0]].append(arr[1])
	World.adjList[arr[1]].append(arr[0])
	World.distance[arr[0]][arr[1]] = arr[2]
	World.distance[arr[1]][arr[0]] = arr[2]
}


////////////////////////////////////////////////////////////////////////////////
// MARK: Main loop
////////////////////////////////////////////////////////////////////////////////

func readEntities(n: Int) {

	world.troops.removeAll(keepingCapacity: true)
	world.bombs.removeAll(keepingCapacity: true)

	for _ in 0..<n {
		let line = rl()!
		let entity = Entity(parseFrom: line)!
		if let factory = Factory(entity: entity) {
			world.factories[factory.id] = factory
		}
		else if let troop = Troop(entity: entity) {
			world.troops.append(troop)
		}
		else if let bomb = Bomb(entity: entity) {
			if bomb.owner == Factory.ownerMe {
				world.bombs.append(bomb)
			}
			else {
				let resolved = bombAdviser.guessTarget(for: bomb, in: world)
				world.bombs.append(resolved)
			}
		}
		else {
			log("Unknown entity=\(entity)")
		}
	}

}


// TODO: expect score
var expectedScore = ""
var expectedOrders = PendingOrders()
for turn in 0..<200 {

	if let line = rl(), let entityCount = Int(line) {

		readEntities(n: entityCount)
		world.turn = turn
	}
	else {
		log("\n--- Stand alone complex @ turn \(turn) ---")
		world = world.nextTurn(with: expectedOrders, options: .wait)
	}

	if feof(stdin) != 0 {
		//log("end of line found")
	}

	// Log score
	let scoreFormat: (World) -> String = { return "\($0.score(owner: 1)):\($0.score(owner: -1))" }
	var score = world.-->scoreFormat
	if score == expectedScore {
		log("score=\(score)")
	}
	else {
		log("score mismatch: expected \(expectedScore), got \(score)")
	}

	// Adjust watchDogTime
	let maxTime = ( turn == 0 )
		? 1000
		: 50
	watchDog.adjustZones(maxTime: maxTime)

	let algorithmist = StrategyAlgorithmHelper(world: world)
	var strategyFactory = StrategyFactory(algorithmHelper: algorithmist)

	let chained = strategyFactory.makeChain()
	expectedOrders = chained.orders
	expectedScore = world.nextTurn(with: expectedOrders, options: .wait).-->scoreFormat

	if expectedOrders.actions.count > 0 {

		print(Action.printableArray(of: chained.orders.actions) + ";MSG \(chained.strategyNames.joined(separator: "-&>"))")
	}
	else {
		log("no strategies available")

		print(Action.wait.description)
	}

	log("measured execution time for turn \(turn) is \(watchDog.millisecondsFromBegin()) ms")
	watchDog.reset()
}

