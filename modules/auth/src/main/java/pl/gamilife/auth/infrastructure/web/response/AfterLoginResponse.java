package pl.gamilife.auth.infrastructure.web.response;

import pl.gamilife.auth.application.dto.LoginUserResult;

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
