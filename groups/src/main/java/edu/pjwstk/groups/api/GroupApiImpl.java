package edu.pjwstk.groups.api;

import edu.pjwstk.api.groups.GroupApi;
import edu.pjwstk.api.groups.dto.GroupDto;
import edu.pjwstk.api.groups.dto.GroupMemberDto;
import edu.pjwstk.groups.usecase.findgroupbyid.FindGroupByIdUseCase;
import edu.pjwstk.groups.usecase.findgroupmemberbyid.FindGroupMemberByIdUseCase;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
public class GroupApiImpl implements GroupApi {

    private final FindGroupMemberByIdUseCase findGroupMemberByIdUseCase;
    private final FindGroupByIdUseCase findGroupByIdUseCase;

    public GroupApiImpl(FindGroupMemberByIdUseCase findGroupMemberByIdUseCase,
                        FindGroupByIdUseCase findGroupByIdUseCase) {
        this.findGroupMemberByIdUseCase = findGroupMemberByIdUseCase;
        this.findGroupByIdUseCase = findGroupByIdUseCase;
    }

    @Override
    public GroupMemberDto findGroupMemberById(UUID groupMemberId) {
        return findGroupMemberByIdUseCase.execute(groupMemberId);
    }

    @Override
    public GroupDto findGroupById(UUID groupId) {
        return findGroupByIdUseCase.execute(groupId);
    }


}
