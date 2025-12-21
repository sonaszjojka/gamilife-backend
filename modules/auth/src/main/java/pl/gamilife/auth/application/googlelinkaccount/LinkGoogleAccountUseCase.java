package pl.gamilife.auth.application.googlelinkaccount;

import pl.gamilife.auth.application.common.LoginUserResult;
import pl.gamilife.shared.kernel.architecture.UseCase;

import java.util.Optional;

public interface LinkGoogleAccountUseCase extends UseCase<LinkGoogleAccountCommand, Optional<LoginUserResult>> {
}
