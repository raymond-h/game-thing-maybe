<template>
  <div />
</template>

<script>
import authService from '../api/auth';

export default {
  mounted() {
    authService.handleCallback()
      .subscribe(
        () => this.$router.replace('/'),
        err => console.error(err)
      );
  }
};
</script>

<style scoped>
</style>
