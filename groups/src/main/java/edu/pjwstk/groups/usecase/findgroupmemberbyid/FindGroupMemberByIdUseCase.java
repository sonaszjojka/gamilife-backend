package edu.pjwstk.groups.usecase.findgroupmemberbyid;

import edu.pjwstk.api.groups.dto.GroupMemberDto;
import edu.pjwstk.core.UseCase;

import java.util.UUID;

public interface FindGroupMemberByIdUseCase extends UseCase<FindGroupMemberByIdCommand, GroupMemberDto> {
}
