package edu.pjwstk.auth.controller.response;

import edu.pjwstk.auth.usecase.common.LoginUserResult;

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
