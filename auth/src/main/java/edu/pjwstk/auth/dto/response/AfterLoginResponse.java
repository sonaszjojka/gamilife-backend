package edu.pjwstk.auth.dto.response;

import edu.pjwstk.auth.dto.service.LoginUserResult;

import java.util.UUID;

public record AfterLoginResponse(
        UUID userId,
        String email,
        String username,
        boolean isEmailVerified) {

    public static AfterLoginResponse from(LoginUserResult result) {
        return new AfterLoginResponse(
                result.userId(),
                result.email(),
                result.username(),
                result.isEmailVerified()
        );
    }
}
