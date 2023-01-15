package components

import "errors"

type ownerList struct {
	// Acts as linked list of predecessors
	predecessorMap map[string]BodyComponentType

	lastest BodyComponentType
}

func NewOwnerList(top BodyComponentType) *ownerList {
	predecessorMap := map[string]BodyComponentType{}

	predecessorMap[top.Name()] = nil

	return &ownerList{
		predecessorMap: predecessorMap,
		lastest:        top,
	}
}

func (ol *ownerList) GetLatest() BodyComponentType {
	return ol.lastest
}

func (ol *ownerList) AddOwner(n BodyComponentType) {
	ol.predecessorMap[n.Name()] = ol.GetLatest()
	ol.lastest = n
}

func (ol *ownerList) AddSuccessorToLatest(n BodyComponentType) {
	ol.predecessorMap[n.Name()] = ol.GetLatest()
}

func (ol *ownerList) GetOwnerOf(n BodyComponentType) (BodyComponentType, error) {

	ret, ok := ol.predecessorMap[n.Name()]
	if !ok {
		return nil, errors.New("No suitable owner found")
	}

	return ret, nil
}