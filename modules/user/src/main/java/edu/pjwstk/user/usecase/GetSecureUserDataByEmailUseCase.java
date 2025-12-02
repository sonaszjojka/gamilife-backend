package edu.pjwstk.user.usecase;

import pl.gamification.api.user.dto.SecureUserInfoApiDto;

import java.util.Optional;

public interface GetSecureUserDataByEmailUseCase {
    Optional<SecureUserInfoApiDto> execute(String email);
}
