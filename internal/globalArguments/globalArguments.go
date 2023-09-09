package globalArguments

var Verbose *bool
var IntSize *int
var Debug *bool

var Sequential *bool

func Init() {
	Verbose = new(bool)
	IntSize = new(int)
	Debug = new(bool)
	Sequential = new(bool)
}
