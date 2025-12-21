package pl.gamilife.auth.application.verifyemail;

import pl.gamilife.auth.application.common.LoginUserResult;
import pl.gamilife.shared.kernel.architecture.UseCase;

public interface VerifyEmailUseCase extends UseCase<VerifyEmailCommand, LoginUserResult> {
}
