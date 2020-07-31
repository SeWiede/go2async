package components

const archPrefix = "beh_"
const defaultArch = "behavioural"

type Component interface {
	Component() string
	Architecture() string
	ArchName() string
}

type BodyComponent interface {
	Component
	InChannel() *HandshakeChannel
	OutChannel() *HandshakeChannel
}
