package edu.pjwstk.auth.util;

public interface ForgotPasswordCodeUtil {
    String generateCode();

    String hashCode(String code);
}
