console.log('hello world!', document.getElementById('root'));

const apiUrl = process.env.API_URL.replace(/{hostname}/g, location.hostname);

fetch(apiUrl + '/test')
  .then(res => res.text())
  .then(text => console.log('From backend:', text))
  .catch(err => console.error(err));
