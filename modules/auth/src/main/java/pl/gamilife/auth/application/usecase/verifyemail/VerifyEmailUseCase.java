package pl.gamilife.auth.application.usecase.verifyemail;

import pl.gamilife.auth.application.dto.LoginUserResult;
import pl.gamilife.shared.kernel.architecture.UseCase;

public interface VerifyEmailUseCase extends UseCase<VerifyEmailCommand, LoginUserResult> {
}
