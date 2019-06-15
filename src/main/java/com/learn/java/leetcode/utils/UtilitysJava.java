package com.learn.java.leetcode.utils;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonParser;

import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;

public class UtilitysJava {
	public static void sort(JsonElement e) {
		if (e.isJsonNull()) {
			return;
		}

		if (e.isJsonPrimitive()) {
			return;
		}

		if (e.isJsonArray()) {
			JsonArray a = e.getAsJsonArray();
			for (Iterator<JsonElement> it = a.iterator(); it.hasNext(); ) {
				sort(it.next());
			}
			return;
		}

		if (e.isJsonObject()) {
			Map<String, JsonElement> tm = new TreeMap<String, JsonElement>(String::compareTo);
			for (Map.Entry<String, JsonElement> en : e.getAsJsonObject().entrySet()) {
				tm.put(en.getKey(), en.getValue());
			}

			for (Map.Entry<String, JsonElement> en : tm.entrySet()) {
				e.getAsJsonObject().remove(en.getKey());
				e.getAsJsonObject().add(en.getKey(), en.getValue());
				sort(en.getValue());
			}
			return;
		}
	}

	/**
	 * 排序Json字符串
	 *
	 * @param json
	 * @return
	 */
	public static String sortJsonObject(String json) {
		JsonParser p = new JsonParser();
		JsonElement e = p.parse(json);

		sort(e);
		return e.toString();
	}
}
