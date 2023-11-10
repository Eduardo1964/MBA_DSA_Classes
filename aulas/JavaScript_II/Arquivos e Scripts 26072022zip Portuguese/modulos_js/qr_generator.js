// npm install easyqrcodejs-nodejs

const QRCode = require('easyqrcodejs-nodejs');

// Options
var options = {
	text: "twitch.tv/xandao_lab"
};

// New instance with options
var qrcode = new QRCode(options);

// Save QRCode image
qrcode.saveImage({
	path: 'q.png' // save path
});