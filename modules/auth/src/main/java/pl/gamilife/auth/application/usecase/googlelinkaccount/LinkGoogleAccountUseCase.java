package pl.gamilife.auth.application.usecase.googlelinkaccount;

import pl.gamilife.auth.application.dto.LoginUserResult;
import pl.gamilife.shared.kernel.architecture.UseCase;

import java.util.Optional;

public interface LinkGoogleAccountUseCase extends UseCase<LinkGoogleAccountCommand, Optional<LoginUserResult>> {
}
