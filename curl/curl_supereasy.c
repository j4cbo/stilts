#include <stdlib.h>
#include <string.h>
#include <curl/curl.h>

typedef struct
{
	char *data;
	size_t len;
}
curl_supereasy_data_t;

size_t curl_supereasy_callback(
	void *buffer,
	size_t size,
	size_t nmemb,
	curl_supereasy_data_t *datat)
{
	size_t old_len = datat->len;
	size_t additional_len = size*nmemb;

	datat->data = realloc(datat->data, old_len + additional_len);
	datat->len = old_len + additional_len;

	memcpy(datat->data + old_len, buffer, additional_len);

	return additional_len;
}

curl_supereasy_data_t* curl_supereasy(char *url)
{
	curl_supereasy_data_t *datat = malloc(sizeof(curl_supereasy_data_t));
	datat->data = NULL;
	datat->len = 0;

	CURL *curl = curl_easy_init();
	curl_easy_setopt(curl, CURLOPT_URL, url);
	curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, curl_supereasy_callback);
	curl_easy_setopt(curl, CURLOPT_WRITEDATA, datat);
	curl_easy_perform(curl);
	curl_easy_cleanup(curl);

	return datat;
}

char* curl_supereasy_data(curl_supereasy_data_t *datat)
{
	return datat->data;
}

size_t curl_supereasy_len(curl_supereasy_data_t *datat)
{
	return datat->len;
}

void curl_supereasy_cleanup(curl_supereasy_data_t *datat)
{
	free(datat->data);
	free(datat);
}
