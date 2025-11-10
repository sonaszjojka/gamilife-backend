package edu.pjwstk.groups.usecase.getgroups;

import edu.pjwstk.groups.enums.GroupTypeEnum;
import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.repository.GroupJpaRepository;
import edu.pjwstk.groups.util.GroupSpecificationBuilder;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Comparator;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
@Slf4j
public class GetGroupsUseCaseImpl implements GetGroupsUseCase {

    private final GroupJpaRepository groupRepository;
    private final GroupSpecificationBuilder specificationBuilder;

    @Override
    @Transactional(readOnly = true)
    public GetGroupsResult executeInternal(GetGroupsCommand cmd) {
        log.debug("Fetching groups with filters: {}", cmd);

        GroupTypeEnum groupType = cmd.type() != null
                ? GroupTypeEnum.fromId(cmd.type())
                : null;

        Page<Group> groupPage = groupRepository.findAll(
                getGroupSpecification(cmd, groupType),
                createPageable(cmd)
        );
        List<UUID> groupIds = groupPage.map(Group::getGroupId).getContent();

        List<Group> groupsWithDetails;
        if (!groupIds.isEmpty()) {
            groupsWithDetails = groupRepository.findWithGroupMembersByGroupIdIn(groupIds);
            groupsWithDetails.sort(Comparator.comparingInt(g -> groupIds.indexOf(g.getGroupId())));
        } else {
            groupsWithDetails = List.of();
        }

        log.debug("Found {} groups", groupPage.getTotalElements());

        return buildGetGroupsResult(groupPage, groupsWithDetails);
    }

    private Specification<Group> getGroupSpecification(GetGroupsCommand cmd, GroupTypeEnum groupType) {
        return specificationBuilder.buildSpecification(
                cmd.joinCode(),
                groupType,
                cmd.name()
        );
    }

    private Pageable createPageable(GetGroupsCommand cmd) {
        return PageRequest.of(
                cmd.page(),
                cmd.size(),
                Sort.by(Sort.Direction.ASC, "name")
        );
    }

    private GetGroupsResult buildGetGroupsResult(Page<Group> groupPage, List<Group> groups) {
        return new GetGroupsResult(
                groupPage.getTotalPages(),
                groupPage.getTotalPages(),
                groupPage.getNumber(),
                groupPage.getSize(),
                groups.stream().map(g -> new GetGroupsResult.GroupDto(
                        g.getGroupId(),
                        g.getJoinCode(),
                        g.getName(),
                        g.getAdminId(),
                        g.getGroupCurrencySymbol(),
                        g.getMembersLimit(),
                        new GetGroupsResult.GroupTypeDto(g.getGroupType().getTitle()),
                        g.getGroupMembers().size()
                )).toList()
        );
    }
}