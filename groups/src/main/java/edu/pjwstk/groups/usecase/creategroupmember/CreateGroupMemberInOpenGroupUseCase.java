package edu.pjwstk.groups.usecase.creategroupmember;

import java.util.UUID;

public interface CreateGroupMemberInOpenGroupUseCase {
    CreateGroupMemberResponse execute(CreateGroupMemberRequest request, UUID groupId);
}
