package edu.pjwstk.groups.api;

import pl.gamilife.api.group.GroupApi;
import pl.gamilife.api.group.dto.FindAllGroupsByUserIdWhereUserIsMemberResult;
import pl.gamilife.api.group.dto.GroupDto;
import pl.gamilife.api.group.dto.GroupMemberDto;
import edu.pjwstk.groups.usecase.findallgroupsbyuserIdwhereuserismemberusecase.FindAllGroupsByUserIdWhereUserIsMemberCommand;
import edu.pjwstk.groups.usecase.findallgroupsbyuserIdwhereuserismemberusecase.FindAllGroupsByUserIdWhereUserIsMemberUseCase;
import edu.pjwstk.groups.usecase.findgroupbyid.FindGroupByIdCommand;
import edu.pjwstk.groups.usecase.findgroupbyid.FindGroupByIdUseCase;
import edu.pjwstk.groups.usecase.findgroupmemberbyid.FindGroupMemberByIdCommand;
import edu.pjwstk.groups.usecase.findgroupmemberbyid.FindGroupMemberByIdUseCase;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
public class GroupApiImpl implements GroupApi {

    private final FindGroupMemberByIdUseCase findGroupMemberByIdUseCase;
    private final FindGroupByIdUseCase findGroupByIdUseCase;
    private final FindAllGroupsByUserIdWhereUserIsMemberUseCase findAllGroupsByUserIdWhereUserIsMember;

    public GroupApiImpl(FindGroupMemberByIdUseCase findGroupMemberByIdUseCase,
                        FindGroupByIdUseCase findGroupByIdUseCase, FindAllGroupsByUserIdWhereUserIsMemberUseCase findAllGroupsByUserIdWhereUserIsMember) {
        this.findGroupMemberByIdUseCase = findGroupMemberByIdUseCase;
        this.findGroupByIdUseCase = findGroupByIdUseCase;
        this.findAllGroupsByUserIdWhereUserIsMember = findAllGroupsByUserIdWhereUserIsMember;
    }

    @Override
    public GroupMemberDto findGroupMemberById(UUID groupMemberId) {
        return findGroupMemberByIdUseCase.execute(
                new FindGroupMemberByIdCommand(groupMemberId)
        );
    }

    @Override
    public GroupDto findGroupById(UUID groupId) {
        return findGroupByIdUseCase.execute(new FindGroupByIdCommand(groupId));
    }

    @Override
    public FindAllGroupsByUserIdWhereUserIsMemberResult findAllGroupsByUserIdWhereUserIsMember(UUID userId, Integer page,
                                                                                               Integer size, String joinCode,
                                                                                               Integer groupType, String groupName) {
        return findAllGroupsByUserIdWhereUserIsMember.execute(
                new FindAllGroupsByUserIdWhereUserIsMemberCommand(
                        userId, page, size, joinCode, groupType, groupName
                ));
    }



}
