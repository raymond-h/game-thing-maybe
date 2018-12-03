<template>
  <div>
    <h1>{{ title }}</h1>
    <div>{{ data }}</div>
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

<style scoped>
</style>
