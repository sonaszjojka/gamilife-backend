package edu.pjwstk.user.usecase;

import edu.pjwstk.api.user.dto.BasicUserInfoApiDto;

import java.util.Optional;
import java.util.UUID;

public interface GetUserByIdUseCase {
    Optional<BasicUserInfoApiDto> execute(UUID userId);
}
