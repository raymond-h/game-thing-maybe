<template>
  <div>
    This is the main page!! {{ userInfo }}
  </div>
</template>

<script>
import authService from '../api/auth';

export default {
  subscriptions() {
    return {
      userInfo: authService.userInfo$
    };
  }
};
</script>

<style scoped>
</style>
