package globalArguments

var Verbose *bool
var IntSize *int
var Debug *bool

func Init() {
	Verbose = new(bool)
	IntSize = new(int)
	Debug = new(bool)
}
