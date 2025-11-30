package edu.pjwstk.groups.usecase.findgroupmemberbyid;

import edu.pjwstk.api.groups.dto.GroupMemberDto;
import edu.pjwstk.core.exception.common.domain.GroupMemberNotFoundException;
import edu.pjwstk.groups.model.GroupMember;
import edu.pjwstk.groups.repository.GroupMemberJpaRepository;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class FindGroupMemberByIdUseCaseImpl implements FindGroupMemberByIdUseCase {

    private final GroupMemberJpaRepository groupMemberRepository;

    @Override
    public GroupMemberDto execute(FindGroupMemberByIdCommand cmd) {
        GroupMember groupMember = groupMemberRepository.findById(cmd.groupMemberId())
                .orElseThrow(() -> new GroupMemberNotFoundException("Group member with id: " +
                        cmd.groupMemberId() + " not found")
                );
        return buildGroupMemberDto(groupMember);
    }

    private GroupMemberDto buildGroupMemberDto(GroupMember groupMember) {
        return GroupMemberDto.builder()
                .groupMemberId(groupMember.getGroupMemberId())
                .memberGroup(new GroupMemberDto.GroupDto(groupMember.getGroupId()))
                .userId(groupMember.getUserId())
                .joinedAt(groupMember.getJoinedAt())
                .leftAt(groupMember.getLeftAt())
                .groupMoney(groupMember.getGroupMoney())
                .totalEarnedMoney(groupMember.getTotalEarnedMoney())
                .build();
    }
}
