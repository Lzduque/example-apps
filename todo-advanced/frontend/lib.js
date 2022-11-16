// Element -> String -> String -> [(Name, Value)] -> Maybe String -> Element
const addElement = (parent, elementType, name, attrs = [], text = null) => {
	const el = document.createElement(elementType)
	el.setAttribute('id', name)
	if (text !== null) el.innerText = text
	attrs.forEach(([n, v]) => el.setAttribute(n, v))
	parent.append(el)
	return el
}
