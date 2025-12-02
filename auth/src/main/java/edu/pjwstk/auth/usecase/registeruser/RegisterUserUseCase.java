package edu.pjwstk.auth.usecase.registeruser;

import pl.gamification.api.user.dto.BasicUserInfoApiDto;
import pl.gamilife.infrastructure.core.architecture.UseCase;

public interface RegisterUserUseCase extends UseCase<RegisterUserCommand, BasicUserInfoApiDto> {
}
