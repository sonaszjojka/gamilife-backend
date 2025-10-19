package edu.pjwstk.groups.usecase.editgroupmember;

import jakarta.validation.Valid;

import java.util.UUID;

public interface EditGroupMemberUseCase {

    EditGroupMemberResponse execute(UUID groupMemberId, EditGroupMemberRequest request);
}
