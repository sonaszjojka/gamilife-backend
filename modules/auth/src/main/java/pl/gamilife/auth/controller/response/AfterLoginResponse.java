package pl.gamilife.auth.controller.response;

import pl.gamilife.auth.usecase.common.LoginUserResult;

import java.util.UUID;

public record AfterLoginResponse(
        UUID userId,
        String email,
        String username,
        boolean isEmailVerified,
        boolean isTutorialCompleted,
        int money
) {

    public static AfterLoginResponse from(LoginUserResult result) {
        return new AfterLoginResponse(
                result.userId(),
                result.email(),
                result.username(),
                result.isEmailVerified(),
                result.isTutorialCompleted(),
                result.money()
        );
    }
}
