package edu.pjwstk.auth.usecase.registeruser;

import edu.pjwstk.api.user.dto.BasicUserInfoApiDto;
import edu.pjwstk.core.UseCase;
import jakarta.validation.Valid;

public interface RegisterUserUseCase extends UseCase<RegisterUserCommand, BasicUserInfoApiDto> {
}
