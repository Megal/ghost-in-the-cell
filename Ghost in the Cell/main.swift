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
func log(_ message: String) {	print(message, to: &errStream) }
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

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// MARK: - ここテン
////////////////////////////////////////////////////////////////////////////////

enum EntityType: String {

	case factory = "FACTORY"
	case troop = "TROOP"
	case bomb = "BOMB"
}

struct Entity {

	var id: Int
	var type: EntityType
	var arg1: Int
	var arg2: Int
	var arg3: Int
	var arg4: Int
	var arg5: Int
}

extension Entity {

	init?(parseFrom line: String) {
		let input = line.components(separatedBy: " ").flatMap { String($0) }
		guard input.count == 7 else { return nil }

		guard let id = Int(input[0]) else { return nil }
		guard let type = EntityType(rawValue: input[1]) else { return nil }
		let args = input.suffix(from: 2).flatMap { Int($0) }
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

/// A kind of Entity, where type == EntityType.factory
struct Factory {

	/// Underlying plain struct `Entity`
	var entity: Entity

	/// Id
	var id: Int {
		get { return entity.id }
		set { entity.id = newValue }
	}
	/// Type, must be EntityType.factory
	var type: EntityType {
		get { return entity.type }
		set { entity.type = newValue }
	}
	/// Player that owns the factory: 1 for you, -1 for your opponent and 0 if neutral
	var owner: Int {
		get { return entity.arg1 }
		set { entity.arg1 = newValue }
	}
	/// Number of cyborgs in the factory
	var units: Int {
		get { return entity.arg2 }
		set { entity.arg2 = newValue }
	}
	/// Factory production (between 0 and 3)
	var productionRate: Int {
		get { return entity.arg3 }
		set { entity.arg3 = newValue }
	}
	/// number of turns before the factory starts producing again 
	/// (0 means that the factory produces normally)
	var disabled: Int {
		get { return entity.arg4 }
		set { entity.arg4 = newValue }
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
		get { return entity.id }
		set { entity.id = newValue }
	}
	/// Type, must be EntityType.troop
	var type: EntityType {
		get { return entity.type }
		set { entity.type = newValue }
	}
	/// Player that owns the troop: 1 for you or -1 for your opponent
	var owner: Int {
		get { return entity.arg1 }
		set { entity.arg1 = newValue }
	}
	/// Identifier of the factory from where the troop leaves
	var u: Int {
		get { return entity.arg2 }
		set { entity.arg2 = newValue}
	}
	/// Identifier of the factory targeted by the troop
	var v: Int {
		get { return entity.arg3 }
		set { entity.arg3 = newValue }
	}
	/// Number of cyborgs in the troop (positive integer)
	var unitCount: Int {
		get { return entity.arg4 }
		set { entity.arg4 = newValue }
	}
	/// remaining number of turns before the troop arrives (positive integer)
	var turnsLeft: Int {
		get { return entity.arg5 }
		set { entity.arg5 = newValue }
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
		get { return entity.id}
		set { entity.id = newValue }
	}
	/// Player that send the bomb: 1 if it is you, -1 if it is your opponent
	var owner: Int {
		get { return entity.arg1 }
		set { entity.arg1 = newValue}
	}
	/// Identifier of the factory from where the bomb is launched
	var source: Int {
		get { return entity.arg2 }
		set { entity.arg2 = newValue }
	}
	/// Identifier of the targeted factory if it's your bomb, -1 otherwise
	var destination: Int {
		get { return entity.arg3 }
		set { entity.arg3 = newValue}
	}
	/// Remaining number of turns before the bomb explodes
	/// (positive integer) if that's your bomb, -1 otherwise
	var turnsLeft: Int {
		get { return entity.arg4 }
		set { entity.arg4 = newValue }
	}

	init?(entity: Entity) {
		guard entity.type == .bomb else { return nil }
		self.entity = entity
	}
}

struct World {

	static var factoryCount = 0 // 7 ≤ factoryCount ≤ 15
	static var linkCount = 0 // 21 ≤ linkCount ≤ 105

	var factories: [Int: Factory] = [:]
	var adjList: [[Int]] = []
	var distance: [[Int]] = []
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

struct PendingOrder {

	/// factory[i] will decrease amount of cyborgs by `value`
	var usedCyborgs: [Int] = [Int](repeating: 0, count: World.factoryCount)

	/// new troops will be created
	var newTroops: [Action] = []

	/// factory[i] upgraded `value` times
	var productionUpgrade: [Int] = [Int](repeating: 0, count: World.factoryCount)

	/// will create a new bomb
	var newBombs: [Action] = []
}

extension World {

	/// Game score. owner is 1 for me and -1 for opponent
	func score(owner: Int) -> Int {

		let inFactories = factories.values
			.filter { $0.owner == owner }
			.map { $0.units }
			.reduce(0, +)
		let inTroops = troops
			.filter { $0.owner == owner }
			.map { $0.unitCount }
			.reduce(0, +)

		return inFactories + inTroops
	}

	/// World state on next turn assuming WAIT action from players
	/// todo: move bombs
	func nextTurn() -> World {

		// ---
		// Move troops and bombs
		// ---
		var newTroops: [Troop] = []
		var troopsReadyToFight: [Troop] = []
		for troop in troops {
			var newTroop = troop
			newTroop.turnsLeft = troop.turnsLeft - 1

			if newTroop.turnsLeft > 0 {
				newTroops.append(newTroop)
			}
			else {
				troopsReadyToFight.append(newTroop)
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
		for factory in factories.values.sorted(by: { $0.id < $1.id }) {
			var newFactory = factory
			newFactory.disabled = factory.disabled > 0
				? factory.disabled - 1
				: 0

			newFactories.append(newFactory)
		}

		// ---
		// Execute orders
		// Nothing to do here
		// ---

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
		for (i, factory) in newFactories.enumerated() {
			var forces = [-1:0, 1:0]
			troopsReadyToFight
				.filter { troop in
					troop.v == i
				}
				.forEach { troop in
					forces[troop.owner]! += troop.unitCount
				}

			// Troops fight with each other first
			let kia = min(forces[-1]!, forces[1]!)
			forces[-1]! -= kia
			forces[1]! -= kia

			// Remaining troops fight with factory defences
			for (owner, units) in forces {
				if factory.owner == owner { // same owner
					newFactories[i].units += units
				}
				else { // fight with units defenging factory
					if units > factory.units { // Attacking forces win, change owner
						newFactories[i].owner = owner
						newFactories[i].units = units - factory.units
					}
					else { // Defences win
						newFactories[i].units -= units
					}
				}
			}
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
			factories: newFactories.indexedDictionary,
			adjList: adjList,
			distance: distance,
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

		let target = world.factories.values
			.filter { target in
				target.id != bomb.source
			}
			.filter { target in
				world.distance[bomb.source][target.id] >= minDuration
			}
			.sorted{ a, b in
				let distA = world.distance[bomb.source][a.id]
				let distB = world.distance[bomb.source][b.id]

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
			resolved.turnsLeft = world.distance[bomb.source][target.id]

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
		for (id, factory) in world.factories {
			if factory.owner == Factory.ownerMe {
				owned.append(id)
			}
		}

		return owned
	}

	/// Factories with neutral owner, but reachable from owned
	lazy var unownedReachable: Set<Int> = { return self.getUnownedReachable() }()
	private func getUnownedReachable() -> Set<Int> {

		var reachable: Set<Int> = []
		for u in self.ownedFactories {
			for v in world.adjList[u] {
				let factory = world.factories[v]!
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
			guard let target = world.factories[v] else { log("expected factory with id=\(v)"); continue }
			guard target.owner == owner else { continue }

			let dist = world.distance[u][v]
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
}

protocol StrategyProtocol {

	var name: String { get }
	var possible: Bool { get }
	var actions: [Action] { get }
}

struct WaitStrategy: StrategyProtocol {

	var possible = true
	var actions = [Action.wait]
	var name: String = "WaitStrategy"
}

struct ExpandToNearest: StrategyProtocol {

	/// Algorithm helper
	let helper: StrategyAlgorithmHelper

	var bestDistance = 999
	var bestEdge: Edge? = nil

	init(helper: StrategyAlgorithmHelper) {
		self.helper = helper

		evalBestEdge()
	}

	mutating func evalBestEdge() {
		for v in helper.unownedReachable {
			for u in helper.ownedFactories {
				let distance = helper.world.distance[u][v]
				if distance < bestDistance {
					bestDistance = distance
					bestEdge = (u,v)
				}
			}
		}
	}

	var possible: Bool {
		return bestEdge != nil
	}

	var actions: [Action] {
		if let (u, v) = bestEdge {
			return [Action.move(u: u, v: v, count: 1)]
		}
		else {
			return [Action.wait]
		}
	}

	var name: String = "ExpandToNearest"
}

struct ExpandAgressively : StrategyProtocol {

	// Algorithm helper
	let helper: StrategyAlgorithmHelper

	init(helper: StrategyAlgorithmHelper) {
		self.helper = helper

		evalActions()
	}

	var possible: Bool { return self.actions.count > 0 }

	var actions: [Action] = []

	var name: String = "ExpandAgressively"

	mutating func evalActions() {
		var visited: Set<Int> = []
		actions = []

		for u in helper.ownedFactories {
			guard let factory = helper.world.factories[u] else { log("missing facory with id=\(u)"); continue }

			var remains = factory.units
			let targetFactories = helper.closestFactories(to: u, owner: Factory.ownerNeutral)
				.sorted { (lhs, rhs) -> Bool in
					return lhs.factory.productionRate == rhs.factory.productionRate
						? lhs.dist < rhs.dist
						: lhs.factory.productionRate < rhs.factory.productionRate
				}
				.map { $0.factory }

			for target in targetFactories {
				guard remains > 0 else { break }

				if target.units < remains {
					let go = target.units > 0
						? target.units
						: 1
					actions.append(Action.move(u: u, v: target.id, count: go))
					remains -= go
					visited.insert(target.id)
				}
			}
		}
	}
}

struct NoRemorse : StrategyProtocol {

	// Algorithm helper
	let helper: StrategyAlgorithmHelper

	init(helper: StrategyAlgorithmHelper) {
		self.helper = helper

		evalActions()
	}

	var possible: Bool { return self.actions.count > 0 }

	var actions: [Action] = []

	var name: String = "NoRemorse"

	mutating func evalActions() {
		var visited: Set<Int> = []
		actions = []

		for u in helper.ownedFactories {
			guard let factory = helper.world.factories[u] else { log("missing facory with id=\(u)"); continue }

			var remains = factory.units
			let targetFactories = helper.closestFactories(to: u, owner: Factory.ownerOpponent)
				.sorted { (lhs, rhs) -> Bool in
					return lhs.factory.productionRate == rhs.factory.productionRate
						? lhs.dist < rhs.dist
						: lhs.factory.productionRate < rhs.factory.productionRate
				}

			/// Calculate more precise count to concuier
			for (target, dist) in targetFactories {
				guard remains > 0 else { break }

				let required = target.units + target.productionRate*dist
				if required < remains {
					let go = required > 0
						? required
						: 1
					actions.append(Action.move(u: u, v: target.id, count: go))
					remains -= go
					visited.insert(target.id)
				}
			}
		}
	}
}

struct StrategyFactory {

	/// Algorithm Helper
	let algorithmHelper: StrategyAlgorithmHelper

	func make() -> [StrategyProtocol] {
		let noRemorse = NoRemorse(helper: algorithmHelper)
		let expandAgro = ExpandAgressively(helper: algorithmHelper)
		let toNearest = ExpandToNearest(helper: algorithmHelper)
		let waiter = WaitStrategy()

		return [
			noRemorse,
			expandAgro,
			toNearest,
			waiter,
		]
	}
}

var strategies: [StrategyProtocol] = []
strategies.append(WaitStrategy())


////////////////////////////////////////////////////////////////////////////////
// MARK: - Main / preinitialization loop
////////////////////////////////////////////////////////////////////////////////

var world = World()
var bombAdviser = BombAdviser()

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
World.linkCount = Int(rl()!)! // 21 ≤ linkCount ≤ 105
world.adjList = [[Int]](repeating: [], count: World.factoryCount)
world.distance = repeatElement(repeatElement(Int.max/2, count: World.factoryCount).map{$0}, count: World.factoryCount).map{$0}

for _ in 0..<World.linkCount {
	let arr = rl()!.components(separatedBy: " ").flatMap { Int($0) }
	guard arr.count == 3 else { fatal("arr=\(arr)") }

	world.adjList[arr[0]].append(arr[1])
	world.adjList[arr[1]].append(arr[0])
	world.distance[arr[0]][arr[1]] = arr[2]
	world.distance[arr[1]][arr[0]] = arr[2]
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
for turn in 0..<200 {

	if let line = rl(), let entityCount = Int(line) {

		readEntities(n: entityCount)
		world.turn = turn
	}
	else {
		log("\n--- Stand alone complex @ turn \(turn) ---")
		world = world.nextTurn()
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
	expectedScore = world.nextTurn().-->scoreFormat

	let algorithmist = StrategyAlgorithmHelper(world: world)
	var strategyFactory = StrategyFactory(algorithmHelper: algorithmist)
	if let strategy = strategyFactory.make().first(where: { $0.possible }) {
		print(Action.printableArray(of: strategy.actions) + ";MSG \(strategy.name)")
	}
	else {
		log("no strategies available")
		print(Action.wait.description)
	}
}


