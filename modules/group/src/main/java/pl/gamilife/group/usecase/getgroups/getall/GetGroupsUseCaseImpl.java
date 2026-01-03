package pl.gamilife.group.usecase.getgroups.getall;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.group.enums.GroupTypeEnum;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.repository.GroupJpaRepository;
import pl.gamilife.group.util.GroupSpecificationBuilder;

import java.util.Comparator;
import java.util.List;
import java.util.UUID;

@Service
@Transactional(readOnly = true)
@RequiredArgsConstructor
@Slf4j
public class GetGroupsUseCaseImpl implements GetGroupsUseCase {

    private final GroupJpaRepository groupRepository;
    private final GroupSpecificationBuilder specificationBuilder;

    @Override
    public GetGroupsResult execute(GetGroupsCommand cmd) {
        log.debug("Fetching groups with filters: {}", cmd);

        GroupTypeEnum groupType = cmd.type() != null
                ? GroupTypeEnum.fromId(cmd.type())
                : null;

        Page<Group> groupPage = groupRepository.findAll(
                getGroupSpecification(cmd, groupType),
                createPageable(cmd)
        );
        List<UUID> groupIds = groupPage.map(Group::getId).getContent();

        List<Group> groupsWithDetails;
        if (!groupIds.isEmpty()) {
            groupsWithDetails = groupRepository.findWithActiveMembersByIdIn(groupIds);
            groupsWithDetails.sort(Comparator.comparingInt(g -> groupIds.indexOf(g.getId())));
        } else {
            groupsWithDetails = List.of();
        }

        log.debug("Found {} groups", groupPage.getTotalElements());

        return buildGetGroupsResult(groupPage, groupsWithDetails);
    }

    private Specification<Group> getGroupSpecification(GetGroupsCommand cmd, GroupTypeEnum groupType) {
        return specificationBuilder.buildSpecification(
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
                groupPage.getTotalElements(),
                groupPage.getTotalPages(),
                groupPage.getNumber(),
                groupPage.getSize(),
                groups.stream().map(g -> new GetGroupsResult.GroupDto(
                        g.getId(),
                        g.getName(),
                        g.getAdminId(),
                        g.getCurrencySymbol(),
                        g.getMembersLimit(),
                        new GetGroupsResult.GroupTypeDto(g.getType().getTitle()),
                        g.getActiveMembers().size()
                )).toList()
        );
    }
}