package edu.pjwstk.groups.usecase.findgroupmemberbyid;

import edu.pjwstk.api.groups.dto.GroupMemberDto;
import pl.gamilife.infrastructure.core.architecture.UseCase;

public interface FindGroupMemberByIdUseCase extends UseCase<FindGroupMemberByIdCommand, GroupMemberDto> {
}
