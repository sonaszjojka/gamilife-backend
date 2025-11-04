package edu.pjwstk.auth.usecase.registeruser;

import edu.pjwstk.api.user.dto.BasicUserInfoApiDto;
import edu.pjwstk.core.UseCase;

public interface RegisterUserUseCase extends UseCase<RegisterUserCommand, BasicUserInfoApiDto> {
}
