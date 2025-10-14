package edu.pjwstk.groups.usecase.findgroupmemberbyid;

import edu.pjwstk.groups.entity.GroupMember;
import edu.pjwstk.common.groupsApi.exception.GroupMemberNotFoundException;
import edu.pjwstk.groups.repository.GroupMemberRepository;
import edu.pjwstk.common.groupsApi.dto.GroupMemberDto;
import edu.pjwstk.groups.shared.GroupMemberMapper;
import org.springframework.stereotype.Component;

@Component
public class FindGroupMemberByIdUseCaseImpl implements FindGroupMemberByIdUseCase {

    private final GroupMemberRepository groupMemberRepository;
    private final GroupMemberMapper groupMemberMapper;

    public FindGroupMemberByIdUseCaseImpl(GroupMemberRepository groupMemberRepository, GroupMemberMapper groupMemberMapper) {
        this.groupMemberRepository = groupMemberRepository;
        this.groupMemberMapper = groupMemberMapper;
    }

    @Override
    public GroupMemberDto execute(Integer groupMemberId) {
        GroupMember groupMember = groupMemberRepository.findById(groupMemberId)
                .orElseThrow(() -> new GroupMemberNotFoundException("Group member with id: " + groupMemberId + " not found"));
        return groupMemberMapper.toResponse(groupMember);
    }
}
