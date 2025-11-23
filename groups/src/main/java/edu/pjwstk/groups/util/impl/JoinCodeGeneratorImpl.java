package edu.pjwstk.groups.util.impl;

import edu.pjwstk.groups.util.JoinCodeGenerator;
import org.springframework.stereotype.Component;

import java.security.SecureRandom;

@Component
public class JoinCodeGeneratorImpl implements JoinCodeGenerator {
    private static final String ALLOWED_CHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
    private static final int DEFAULT_CODE_LENGTH = 20;
    private static final SecureRandom RANDOM = new SecureRandom();

    public String generate() {
        return generate(DEFAULT_CODE_LENGTH);
    }

    public String generate(int length) {
        if (length <= 0) {
            throw new IllegalArgumentException("Length must be positive");
        }

        StringBuilder sb = new StringBuilder(length);
        for (int i = 0; i < length; i++) {
            int index = RANDOM.nextInt(ALLOWED_CHARS.length());
            sb.append(ALLOWED_CHARS.charAt(index));
        }
        return sb.toString();
    }
}
