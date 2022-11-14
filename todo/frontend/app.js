const signedIn = false

const showSignInView = () => {
	const app = document.getElementById('app')

	const content = document.createElement('content')
	content.setAttribute('id', 'content')
	app.append(content)

	// REGISTER
	const register = document.createElement('h2')
	register.setAttribute('id', 'register')
	register.innerText = 'Register'
	content.append(register)

	const registerDiv = document.createElement('div')
	registerDiv.setAttribute('id', 'register-div')
	content.append(registerDiv)

	const emailDiv1 = document.createElement('div')
	emailDiv1.setAttribute('id', 'email-div1')
	registerDiv.append(emailDiv1)

	const emailLabel1 = document.createElement('label')
	emailLabel1.setAttribute('id', 'email-label1')
	emailLabel1.innerText = 'Email'
	emailDiv1.append(emailLabel1)

	const emailField1 = document.createElement('input')
	emailField1.setAttribute('id', 'email-field1')
	emailField1.setAttribute('type', 'email')
	emailDiv1.append(emailField1)

	const passwordDiv1 = document.createElement('div')
	passwordDiv1.setAttribute('id', 'password-div1')
	content.append(passwordDiv1)

	const passwordLabel1 = document.createElement('label')
	passwordLabel1.setAttribute('id', 'password-label1')
	passwordLabel1.innerText = 'Password'
	passwordDiv1.append(passwordLabel1)

	const passwordField1 = document.createElement('input')
	passwordField1.setAttribute('id', 'password-field1')
	passwordField1.setAttribute('type', 'password')
	passwordDiv1.append(passwordField1)

	// SIGN IN
	const signin = document.createElement('h2')
	signin.setAttribute('id', 'signin')
	signin.innerText = 'Sign In'
	content.append(signin)

	const signinDiv = document.createElement('div')
	signinDiv.setAttribute('id', 'signin-div')
	content.append(signinDiv)

	const emailDiv2 = document.createElement('div')
	emailDiv2.setAttribute('id', 'email-div2')
	signinDiv.append(emailDiv2)

	const emailLabel2 = document.createElement('label')
	emailLabel2.setAttribute('id', 'email-label2')
	emailLabel2.innerText = 'Email'
	emailDiv2.append(emailLabel2)

	const emailField2 = document.createElement('input')
	emailField2.setAttribute('id', 'email-field2')
	emailField2.setAttribute('type', 'email')
	emailDiv2.append(emailField2)

	const passwordDiv2 = document.createElement('div')
	passwordDiv2.setAttribute('id', 'password-div2')
	content.append(passwordDiv2)

	const passwordLabel2 = document.createElement('label')
	passwordLabel2.setAttribute('id', 'password-label2')
	passwordLabel2.innerText = 'Password'
	passwordDiv2.append(passwordLabel2)

	const passwordField2 = document.createElement('input')
	passwordField2.setAttribute('id', 'password-field2')
	passwordField2.setAttribute('type', 'password')
	passwordDiv2.append(passwordField2)
}

const showListView = () => {}

window.onload = (event) => {
	if (signedIn) {
		showListView()
	} else {
		showSignInView()
	}
}
