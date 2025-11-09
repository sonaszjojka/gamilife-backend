package edu.pjwstk.groups.usecase.leavegroup;

import java.util.UUID;

public interface LeaveGroupUseCase {
    LeaveGroupResult execute(UUID groupMemberId, UUID groupId);

}
