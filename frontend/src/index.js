console.log('hello world!', document.getElementById('root'));

const apiUrl = process.env.API_URL.replace(/{hostname}/g, location.hostname);

fetch(apiUrl + '/some-json')
  .then(res => res.json())
  .then(json => console.log('From backend:', json))
  .catch(err => console.error(err));
