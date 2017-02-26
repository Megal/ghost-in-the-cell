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

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// MARK: - ここテン
////////////////////////////////////////////////////////////////////////////////

enum EntityType: String {

	case factory = "FACTORY"
	case troop = "TROOP"
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
	var cyborgCount: Int {
		get { return entity.arg2 }
		set { entity.arg2 = newValue }
	}
	/// Factory production (between 0 and 3)
	var productionRate: Int {
		get { return entity.arg3 }
		set { entity.arg3 = newValue }
	}

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

// MARK: - Main / preinitialization loop

var factories: [Int: Factory] = [:]
var adjList: [Int: Int] = [:]
var distance: [[Int]] = []
var troops: [Troop] = []



var turn = 0
// turn 0

func rl() -> String? {
	if let line = readLine() {
	//	log(line)
		return line
	}
	else {
		return nil
	}
}

let factoryCount = Int(rl()!)! // 7 ≤ factoryCount ≤ 15
let linkCount = Int(rl()!)! // 21 ≤ linkCount ≤ 105
distance = repeatElement(repeatElement(-1, count: factoryCount).map{$0}, count: factoryCount).map{$0}

for _ in 0..<linkCount {
	let arr = rl()!.components(separatedBy: " ").flatMap { Int($0) }
	guard arr.count == 3 else { fatal("arr=\(arr)") }

	adjList[arr[0]] = arr[1]
	adjList[arr[1]] = arr[0]
	distance[arr[0]][arr[1]] = arr[2]
	distance[arr[1]][arr[0]] = arr[2]
}

// MARK: Main loop

func readEntities(n: Int) {

	troops.removeAll(keepingCapacity: true)
	for _ in 0..<n {
		let line = rl()!
		let entity = Entity(parseFrom: line)!
		if let factory = Factory(entity: entity) {
			factories[factory.id] = factory
		}
		else if let troop = Troop(entity: entity) {
			troops.append(troop)
		}
		else {
			log("Unknown entity=\(entity)")
		}
	}

}


for turn in 0..<200 {

	if let line = rl(), let entityCount = Int(line) {

		readEntities(n: entityCount)
	}

	if feof(stdin) != 0 {
		//log("end of line found")
	}

	print("WAIT")
}

