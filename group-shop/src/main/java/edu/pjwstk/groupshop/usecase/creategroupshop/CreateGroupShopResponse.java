package edu.pjwstk.groupshop.usecase.creategroupshop;

import lombok.Builder;

import java.util.UUID;

@Builder
public record CreateGroupShopResponse(UUID groupShopId,
                                       String name,
                                       String description,
                                       UUID groupId) { }
