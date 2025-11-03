package edu.pjwstk.auth.validators;

import jakarta.validation.Constraint;
import jakarta.validation.Payload;

import java.lang.annotation.*;

@Target({ElementType.FIELD, ElementType.PARAMETER, ElementType.RECORD_COMPONENT})
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = PasswordValidator.class)
@Documented
public @interface SecurePassword {
    String message() default "Password must be at least 8 characters long, contain at least 1 letter, 1 digit and 1 special character";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};
}
