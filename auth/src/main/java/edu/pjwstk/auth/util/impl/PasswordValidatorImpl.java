package edu.pjwstk.auth.util.impl;

import edu.pjwstk.auth.util.PasswordValidator;
import org.springframework.stereotype.Component;

@Component
public class PasswordValidatorImpl implements PasswordValidator {
    @Override
    public boolean validate(String password) {
        if (password == null || password.length() < 8) {
            return false;
        }

        boolean hasSpecial = password.matches(".*[!@#$%^&*()_+\\-={}:;\"'\\[\\]|<>,.?/~`].*");
        boolean hasLetter = password.matches(".*[A-Za-z].*");
        boolean hasDigit = password.matches(".*\\d.*");

        return hasSpecial && hasLetter && hasDigit;
    }
}
