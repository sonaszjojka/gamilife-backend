package edu.pjwstk.groups.usecase.getgrouprequests;

import pl.gamification.api.user.UserApi;
import pl.gamification.api.user.dto.BasicUserInfoApiDto;
import edu.pjwstk.groups.enums.GroupRequestStatusEnum;
import edu.pjwstk.groups.model.GroupRequest;
import edu.pjwstk.groups.repository.GroupRequestJpaRepository;
import edu.pjwstk.groups.util.GroupRequestSpecificationBuilder;
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
@Transactional(readOnly = true)
@RequiredArgsConstructor
@Slf4j
public class GetGroupRequestsUseCaseImpl implements GetGroupRequestsUseCase {

    private final GroupRequestJpaRepository groupRequestRepository;
    private final GroupRequestSpecificationBuilder specificationBuilder;
    private final UserApi userApi;

    @Override
    public GetGroupRequestsResult execute(GetGroupRequestsCommand cmd) {
        log.debug("Fetching group requests with filters: {}", cmd);

        GroupRequestStatusEnum statusEnum = cmd.statusId() != null
                ? GroupRequestStatusEnum.fromId(cmd.statusId())
                : null;

        Page<GroupRequest> requestPage = groupRequestRepository.findAll(
                getGroupRequestSpecification(cmd, statusEnum),
                createPageable(cmd)
        );

        List<UUID> requestIds = requestPage.map(GroupRequest::getGroupRequestId).getContent();

        List<GroupRequest> requestsWithDetails;
        if (!requestIds.isEmpty()) {
            requestsWithDetails = groupRequestRepository.findWithStatusByGroupRequestIdIn(requestIds);
            requestsWithDetails.sort(Comparator.comparingInt(r -> requestIds.indexOf(r.getGroupRequestId())));
        } else {
            requestsWithDetails = List.of();
        }

        log.debug("Found {} group requests", requestPage.getTotalElements());

        return buildGetGroupRequestsResult(requestPage, requestsWithDetails);
    }

    private Specification<GroupRequest> getGroupRequestSpecification(GetGroupRequestsCommand cmd, GroupRequestStatusEnum statusEnum) {
        return specificationBuilder.buildSpecification(
                cmd.groupId(),
                statusEnum
        );
    }

    private Pageable createPageable(GetGroupRequestsCommand cmd) {
        return PageRequest.of(
                cmd.page(),
                cmd.size(),
                Sort.by(Sort.Direction.DESC, "createdAt")
        );
    }

    private GetGroupRequestsResult buildGetGroupRequestsResult(Page<GroupRequest> requestPage, List<GroupRequest> requests) {
        return new GetGroupRequestsResult(
                requestPage.getTotalElements(),
                requestPage.getTotalPages(),
                requestPage.getNumber(),
                requestPage.getSize(),
                requests.stream().map(r -> new GetGroupRequestsResult.GroupRequestDto(
                        r.getGroupRequestId(),
                        r.getUserId(),
                        getUserUsername(r.getUserId()),
                        r.getGroupId(),
                        r.getCreatedAt(),
                        new GetGroupRequestsResult.GroupRequestStatusDto(
                                r.getGroupRequestStatus().getGroupRequestStatusId(),
                                r.getGroupRequestStatus().getTitle()
                        )
                )).toList()
        );
    }

    private String getUserUsername(UUID userId) {
        return userApi.getUserById(userId)
                .map(BasicUserInfoApiDto::username)
                .orElse(null);
    }
}
