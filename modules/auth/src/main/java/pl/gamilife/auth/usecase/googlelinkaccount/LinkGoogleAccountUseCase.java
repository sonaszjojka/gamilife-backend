package pl.gamilife.auth.usecase.googlelinkaccount;

import pl.gamilife.auth.usecase.common.LoginUserResult;
import pl.gamilife.shared.kernel.architecture.UseCase;

import java.util.Optional;

public interface LinkGoogleAccountUseCase extends UseCase<LinkGoogleAccountCommand, Optional<LoginUserResult>> {
}
