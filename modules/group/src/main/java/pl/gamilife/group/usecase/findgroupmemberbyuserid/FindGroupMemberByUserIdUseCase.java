package pl.gamilife.group.usecase.findgroupmemberbyuserid;

import pl.gamilife.shared.kernel.architecture.UseCase;

import java.util.Optional;

public interface FindGroupMemberByUserIdUseCase extends UseCase<FindGroupMemberByUserIdCommand, Optional<FindGroupMemberByUserIdResult>> {
}
