package edu.pjwstk.auth.validators;

import jakarta.validation.ValidationException;
import org.springframework.stereotype.Component;

@Component
public class PasswordValidator {

    public void validate(String password) {
        if (password == null || password.length() < 8) {
            throw new ValidationException("Password length less than 8 characters");
        }

        boolean hasLetter = password.matches(".*[A-Za-z].*");
        boolean hasDigit = password.matches(".*\\d.*");
        boolean hasSpecial = password.matches(".*[!@#$%^&*()_+\\-={}:;\"'\\[\\]|<>,.?/~`].*");

        if (!hasLetter || !hasDigit || !hasSpecial) {
            throw new ValidationException("Password must contain at least one letter, one digit, and one special character");
        }
    }
}
