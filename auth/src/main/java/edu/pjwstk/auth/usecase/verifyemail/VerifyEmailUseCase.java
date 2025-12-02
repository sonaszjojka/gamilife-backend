package edu.pjwstk.auth.usecase.verifyemail;

import edu.pjwstk.auth.usecase.common.LoginUserResult;
import pl.gamilife.infrastructure.core.architecture.UseCase;

public interface VerifyEmailUseCase extends UseCase<VerifyEmailCommand, LoginUserResult> {
}
