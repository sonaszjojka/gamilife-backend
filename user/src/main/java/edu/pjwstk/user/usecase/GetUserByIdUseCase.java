package edu.pjwstk.user.usecase;

import edu.pjwstk.common.userApi.dto.BasicUserInfoApiDto;

import java.util.Optional;
import java.util.UUID;

public interface GetUserByIdUseCase {
    Optional<BasicUserInfoApiDto> execute(UUID userId);
}
