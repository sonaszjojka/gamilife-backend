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

@Service
@RequiredArgsConstructor
@Slf4j
public class GetGroupsUseCaseImpl implements GetGroupsUseCase {

    private final GroupJpaRepository groupRepository;
    private final GroupSpecificationBuilder specificationBuilder;

    @Override
    @Transactional(readOnly = true)
    public Page<GetGroupsResult> executeInternal(GetGroupsCommand cmd) {
        log.debug("Fetching groups with filters: {}", cmd);

        GroupTypeEnum groupType = cmd.type() != null
                ? GroupTypeEnum.fromId(cmd.type())
                : null;

        Page<Group> groups = groupRepository.findWithGroupMembersAll(
                getGroupSpecification(cmd, groupType),
                createPageable(cmd)
        );

        log.debug("Found {} groups", groups.getTotalElements());

        return groups.map(group -> new GetGroupsResult(
                group.getGroupId(),
                group.getJoinCode(),
                group.getName(),
                group.getAdminId(),
                group.getGroupCurrencySymbol(),
                group.getMembersLimit(),
                new GetGroupsResult.GroupTypeDto(group.getGroupType().getTitle()),
                group.getGroupMembers().size()
        ));
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
}