package edu.pjwstk.groups.usecase.leavegroup;

import java.util.UUID;

public interface LeaveGroupUseCase {
    LeaveGroupResponse execute(UUID groupMemberId, UUID groupId);

}
