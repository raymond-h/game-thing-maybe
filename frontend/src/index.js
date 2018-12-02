console.log('hello world!', document.getElementById('root'));

const apiUrl = process.env.API_URL.replace(/{hostname}/g, location.hostname);

async function main() {
  const res = await fetch(apiUrl + '/some-json');

  const json = await res.json();

  console.log('From backend:', json);
}

main()
  .catch(err => console.error(err));
