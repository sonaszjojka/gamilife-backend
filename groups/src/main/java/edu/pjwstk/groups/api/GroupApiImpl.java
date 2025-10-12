package edu.pjwstk.groups.api;

import edu.pjwstk.common.groupsApi.GroupApi;
import edu.pjwstk.common.groupsApi.dto.GroupMemberDto;
import edu.pjwstk.groups.usecase.findgroupmemberbyid.FindGroupMemberByIdUseCase;
import org.springframework.stereotype.Service;

@Service
public class GroupApiImpl implements GroupApi {

    private final FindGroupMemberByIdUseCase findGroupMemberByIdUseCase;

    public GroupApiImpl(FindGroupMemberByIdUseCase findGroupMemberByIdUseCase) {
        this.findGroupMemberByIdUseCase = findGroupMemberByIdUseCase;
    }

    @Override
    public GroupMemberDto findGroupMemberById(Integer groupMemberId) {
        return findGroupMemberByIdUseCase.execute(groupMemberId);
    }
}
