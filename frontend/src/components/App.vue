<template>
  <div class="container">
    <h1 class="title-bar">
      {{ title }}
    </h1>
    <div class="contents">
      {{ data }}
    </div>
  </div>
</template>

<script>
const apiUrl = process.env.API_URL.replace(/{hostname}/g, location.hostname);

async function callApi() {
  const res = await fetch(apiUrl + '/some-json');

  const json = await res.json();

  console.log('From backend:', json);

  return json;
}

export default {
  data() {
    return { title: 'hello world', data: null };
  },

  mounted() {
    callApi().then(data => this.data = data);
  }
};
</script>

<style>
body {
  margin: 0;
}
</style>

<style scoped>
.container {
  width: 100vw;
  height: 100vh;
  display: grid;
  grid-template-columns: auto minmax(auto, 1000px) auto;
  grid-template-rows: 50px auto;
}

.title-bar {
  color: red;
  background: rgb(0, 255, 0);
  margin: 0;
  grid-column-start: 2;
  grid-row-start: 1;
}

.contents {
  background: rgb(127, 127, 255);
  grid-column-start: 2;
  grid-row-start: 2;
}
</style>
