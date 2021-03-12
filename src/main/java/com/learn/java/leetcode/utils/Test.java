package com.learn.java.leetcode.utils;

public class Test {
	private static boolean isEquals(Integer t1,Integer t2){
		return t1 == t2;
	}

	private static boolean isEquals(String s1,String s2){
		return s1 == s2;
	}


	public static void main(String[] args) {
		System.out.println(isEquals(421,421));

		System.out.println(isEquals(11,11));

		System.out.println(isEquals("421","421"));

		System.out.println(isEquals("11","11"));

		System.out.println(isEquals("421","42"+1));

		System.out.println(isEquals(new String("421"),new String("421")));

		System.out.println(isEquals(new String("1"),new String("1")));
	}
}
