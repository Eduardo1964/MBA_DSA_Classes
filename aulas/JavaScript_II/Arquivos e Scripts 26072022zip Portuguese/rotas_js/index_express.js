const express = require('express');

var app = express();

// Convertendo o corpo de requisições em json
app.use(express.json());

app.get('/ola', function(req, res) {
  res.send('Olá Mundo!');
});

app.post('/mensagem', function(req, res) {
  var { mensagem } = req.body
  console.clear();
  console.log(mensagem);
  res.send('Recebido!');
});

app.listen(3000, function() {
  console.log('App de Exemplo escutando na porta 3000!');
});