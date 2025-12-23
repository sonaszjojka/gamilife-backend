package pl.gamilife.user.usecase.getcurrentuserdatetime;

import pl.gamilife.shared.kernel.architecture.UseCase;

import java.time.LocalDateTime;

public interface GetCurrentUserDateTimeUseCase extends UseCase<GetCurrentUserDateTimeCommand, LocalDateTime> {
}
