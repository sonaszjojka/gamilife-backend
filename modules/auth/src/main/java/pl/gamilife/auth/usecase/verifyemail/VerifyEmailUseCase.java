package pl.gamilife.auth.usecase.verifyemail;

import pl.gamilife.auth.usecase.common.LoginUserResult;
import pl.gamilife.infrastructure.core.architecture.UseCase;

public interface VerifyEmailUseCase extends UseCase<VerifyEmailCommand, LoginUserResult> {
}
