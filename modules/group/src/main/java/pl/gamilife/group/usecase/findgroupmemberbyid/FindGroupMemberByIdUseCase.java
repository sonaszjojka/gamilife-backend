package pl.gamilife.group.usecase.findgroupmemberbyid;

import pl.gamilife.api.group.dto.GroupMemberDto;
import pl.gamilife.shared.kernel.architecture.UseCase;

public interface FindGroupMemberByIdUseCase extends UseCase<FindGroupMemberByIdCommand, GroupMemberDto> {
}
